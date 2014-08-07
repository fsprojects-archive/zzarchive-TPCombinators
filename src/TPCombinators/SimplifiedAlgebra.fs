// A simplified view of the algebra of provided constructs.
//
// TODO: Implement ISimpleTypeProvider --> ITypeProvider (injecting implementations of the 'crud')
// TODO: Implement ITypeProvider --> ISimpleTypeProvider (eliding the 'crud')

module FSharp.ProvidedTypes.SimplifiedAlgebra

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open FSharp.ProvidedTypes.GeneralCombinators

[<AutoOpen>]
module Utils = 
    let nullToOption x = match x with null -> None | _ -> Some x
    let optionToNull x = match x with None -> null | Some x -> x

    // These are the exact flags passed in by the F# compiler wherever 'bindingFlags' is expected
    // See https://github.com/fsharp/fsharp/blob/99edb9b717d28c0b23af9a6ac6e8b9a978eee005/src/fsharp/est.fs#L455 for example
    let bindingFlags = BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public
    let bindingFlagsWithoutDeclaredOnly = BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.Public

type ISimpleStaticParameter  = 
    abstract Name : string
    abstract ParameterType : Type // simple types only allowed here
    abstract OptionalValue : obj option
    abstract CustomAttributes : seq<CustomAttributeData>

and ISimpleParameter  = 
    abstract Name : string
    abstract ParameterType : ISimpleType 
    abstract OptionalValue : obj option
    abstract IsOut : bool
    abstract CustomAttributes : seq<CustomAttributeData>
 
and ISimpleConstructor = 
    //abstract DeclaringType    : ISimpleTypeDefinition
    abstract Parameters       : ISimpleParameter[]
    abstract CustomAttributes : seq<CustomAttributeData>

    // Unlike GetInvokerExpression the inputs are variables and have types which are representation types (to be checked)
    abstract GetImplementation : Var[] -> Expr


and ISimpleMethod =
    abstract Name              : string
    abstract Parameters        : ISimpleParameter[]
    abstract ReturnType        : ISimpleType 
    abstract CustomAttributes  : seq<CustomAttributeData>
    abstract IsStatic          : bool
    
    // Unlike GetInvokerExpression the inputs are variables and have types which are representation types (to be checked)
    abstract GetImplementation : Var[] -> Expr

and ISimpleProperty = 
    abstract Name            : string 
    abstract IsStatic        : bool
    abstract PropertyType    : ISimpleType
    abstract GetMethod       : ISimpleAssociatedMethod option
    abstract SetMethod       : ISimpleAssociatedMethod option
    abstract IndexParameters : ISimpleParameter[]
    abstract CustomAttributes : seq<CustomAttributeData>

and ISimpleAssociatedMethod =
    // Unlike GetInvokerExpression the inputs are variables and have types which are representation types (to be checked)
    abstract GetImplementation : Var[] -> Expr

and ISimpleEvent = 
    abstract Name : string
    abstract IsStatic        : bool
    abstract EventHandlerType : ISimpleType
    abstract AddMethod : ISimpleAssociatedMethod
    abstract RemoveMethod : ISimpleAssociatedMethod
    abstract CustomAttributes : seq<CustomAttributeData>

and ISimpleLiteralField  = 
    abstract Name : string  
    abstract FieldType : ISimpleType
    abstract LiteralValue : obj 
    abstract CustomAttributes : seq<CustomAttributeData>

/// Algebra of types
and ISimpleType = 
    | TyApp of ISimpleTypeDefinitionReference * ISimpleType[]
    | TyArray of int * ISimpleType
    | TyPointer of ISimpleType
    | TyByRef of ISimpleType

and ISimpleTypeDefinitionReference = 
    | SimpleTyDef of ISimpleTypeDefinition
    | OtherTyDef of Type

and ISimpleTypeDefinition =
    abstract Name : string
    abstract Assembly : System.Reflection.Assembly
    abstract Namespace : string option
    abstract DeclaringType : ISimpleTypeDefinition option // needed if this is nested

    abstract BaseType : ISimpleType option
    abstract Interfaces : ISimpleType[]

    abstract Constructors : ISimpleConstructor[]
    abstract Methods : ISimpleMethod[]
    abstract Fields : ISimpleLiteralField[]
    abstract Events : ISimpleEvent[]
    abstract Properties : ISimpleProperty[]
    abstract NestedTypes : ISimpleTypeDefinition[]

    // TODO: consider removing these from the simplified model
    //abstract GetField : name:string -> ISimpleLiteralField option
    //abstract GetEvent: name:string -> ISimpleEvent option
    //abstract GetProperty : name:string -> ISimpleProperty option
    abstract GetNestedType : name:string * declaredOnly: bool -> ISimpleTypeDefinition option
    
    abstract StaticParameters : ISimpleStaticParameter[]
    abstract ApplyStaticArguments : string[] * obj[] -> ISimpleTypeDefinition  
    abstract CustomAttributes : seq<CustomAttributeData>
    //abstract UnderlyingSystemType = inp.UnderlyingSystemType |> TxTypeSymbol


and ISimpleNamespace = 
    abstract NestedNamespaces : ISimpleNamespace[]
    abstract NamespaceName : string
    abstract TypeDefinitions : ISimpleTypeDefinition[]
    abstract GetTypeDefinition : name:string -> ISimpleTypeDefinition option

and ISimpleTypeProvider = 
    abstract Namespaces : ISimpleNamespace[]
    [<CLIEvent>]
    abstract Invalidate : IEvent<unit>
    inherit System.IDisposable 




// ----------------------------------------------------------------
// ITypeProvider --> ISimpleTypeProvider



/// Clones namespaces, type providers, types and members provided by tp, renaming namespace nsp1 into namespace nsp2.
let Simplify(tp: ITypeProvider) = 

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    // Unique wrapped type definition objects must be translated to unique wrapper objects, based 
    // on object identity.
    let txTable = TxTable<Type, ISimpleTypeDefinition>()

    let TxCustomAttributes (inp: seq<CustomAttributeData>) = inp

    let rec TxStaticParameter(inp: ParameterInfo) = 

        { new ISimpleStaticParameter with 
            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType 
            override __.OptionalValue = (if inp.IsOptional then Some inp.RawDefaultValue else None)
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
        }

    and TxParameter(inp : ParameterInfo) = 
        { new ISimpleParameter with 
            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = (if inp.IsOptional then Some inp.RawDefaultValue else None)
            override __.IsOut = inp.IsOut 
            override __.CustomAttributes = inp.GetCustomAttributesData()  |> TxCustomAttributes
        }
 
    and TxConstructor(inp: ConstructorInfo) = 
        { new ISimpleConstructor with
            override __.Parameters = inp.GetParameters() |> Array.map TxParameter
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
            override __.GetImplementation(parametersAfterErasure) = TxGetImplementation(inp, parametersAfterErasure)
        }

    and TxMethod(inp: MethodInfo) =
        { new ISimpleMethod with 
            override __.Name              = inp.Name  
            override __.IsStatic          = inp.IsStatic
            override __.Parameters        = inp.GetParameters() |> Array.map TxParameter
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
            override __.GetImplementation(parametersAfterErasure) = TxGetImplementation(inp, parametersAfterErasure)
        }

    and TxGetImplementation (inp: MethodBase, parametersAfterErasure) =
        let parametersBeforeErasure = Array.map Expr.Var parametersAfterErasure 
        tp.GetInvokerExpression(inp,  parametersBeforeErasure)

    and TxAssociatedMethod (inp: MethodBase) =
       { new ISimpleAssociatedMethod with 
            override __.GetImplementation(parametersAfterErasure) = TxGetImplementation(inp, parametersAfterErasure)
       }


    and TxProperty(inp: PropertyInfo) = 
        { new ISimpleProperty with 
            override __.Name = inp.Name 
            override __.IsStatic = (if inp.CanRead then (inp.GetGetMethod()).IsStatic elif inp.CanWrite then inp.GetSetMethod().IsStatic else false)
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = if inp.CanRead then Some (inp.GetGetMethod() |> TxAssociatedMethod) else None
            override __.SetMethod = if inp.CanWrite then Some (inp.GetSetMethod() |> TxAssociatedMethod) else None
            override __.IndexParameters = inp.GetIndexParameters() |> Array.map TxParameter
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
        }

    and TxEventDefinition(inp: EventInfo) = 
        { new ISimpleEvent with 
            override __.Name = inp.Name 
            override __.IsStatic = inp.AddMethod.IsStatic
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxAssociatedMethod
            override __.RemoveMethod = inp.RemoveMethod |> TxAssociatedMethod
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
        }

    and TxField(inp: FieldInfo) = 
        { new ISimpleLiteralField with 
            override __.Name = inp.Name 
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = (if inp.IsLiteral then inp.GetRawConstantValue() else null)
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes
        }

    and TxType(inp: Type) = 
        if inp.IsGenericType then TyApp(TxTypeDefinitionReference (inp.GetGenericTypeDefinition()), Array.map TxType (inp.GetGenericArguments()))
        elif inp.IsArray then TyArray(inp.GetArrayRank(), TxType (inp.GetElementType()))
        elif inp.IsPointer then TyPointer(TxType (inp.GetElementType()))
        elif inp.IsByRef then TyByRef(TxType (inp.GetElementType()))
        else TyApp(TxTypeDefinitionReference inp, [| |])


    and TxTypeDefinitionReference(inp: Type) =
        let isTarget =  (inp.Attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased) <> enum 0)
        if isTarget then SimpleTyDef (TxTypeDefinition inp)
        else OtherTyDef inp

    and TxTypeDefinition(inp: Type) =
      txTable.Get inp <| fun () ->

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly 
            override __.Namespace = nullToOption inp.Namespace 
            override __.DeclaringType = inp.DeclaringType |> nullToOption|> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> nullToOption |> Option.map TxType
            override __.Interfaces = inp.GetInterfaces() |> Array.map TxType

            override __.Constructors = inp.GetConstructors(bindingFlags) |> Array.map TxConstructor
            override __.Methods = 
                let methodToRemove = 
                    set [ for p in inp.GetProperties(bindingFlags) do
                            if p.CanRead then yield p.GetGetMethod().Name 
                            if p.CanWrite then yield p.GetSetMethod().Name 
                          for e in inp.GetEvents(bindingFlags) do
                            yield e.GetAddMethod().Name 
                            yield e.GetRemoveMethod().Name 
                         ]

                [| for m in inp.GetMethods(bindingFlags) do 
                      if not (methodToRemove.Contains m.Name) then 
                           yield TxMethod m |]

            override __.Fields = inp.GetFields(bindingFlags) |> Array.map TxField
            override __.Events = inp.GetEvents(bindingFlags) |> Array.map TxEventDefinition
            override __.Properties = inp.GetProperties(bindingFlags) |> Array.map TxProperty
            override __.NestedTypes = inp.GetNestedTypes(bindingFlags) |> Array.map TxTypeDefinition

            //override __.GetField(name) = inp.GetField(name, bindingFlags) |> nullToOption |> Option.map TxField
            //override __.GetEvent(name) = inp.GetEvent(name, bindingFlags) |> nullToOption |> Option.map TxEventDefinition
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, (if declaredOnly then bindingFlags else bindingFlagsWithoutDeclaredOnly)) |> nullToOption |> Option.map TxTypeDefinition
            //override __.GetProperty(name) = inp.GetProperty(name, bindingFlags) |> nullToOption |> Option.map TxProperty

            //override __.UnderlyingSystemType = inp.UnderlyingSystemType |> TxType
            override __.CustomAttributes = inp.GetCustomAttributesData() |> TxCustomAttributes

            override __.ApplyStaticArguments(typePathWithArguments, objs) = tp.ApplyStaticArguments(inp, typePathWithArguments, objs) |> TxTypeDefinition

            override __.StaticParameters = tp.GetStaticParameters(inp) |> Array.map TxStaticParameter

        }

    /// Transform a provided member definition
    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: IProvidedNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.GetNestedNamespaces() |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName 
            override __.TypeDefinitions = inp.GetTypes() |> Array.map TxTypeDefinition
            override __.GetTypeDefinition(name) =  inp.ResolveTypeName(name) |> nullToOption |> Option.map TxTypeDefinition
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ITypeProvider) = 
        { new ISimpleTypeProvider with 
            override __.Namespaces = inp.GetNamespaces() |> Array.map TxNamespaceDefinition


            [<CLIEvent>]
            override __.Invalidate = inp.Invalidate |> Event.map (fun _ -> ())

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
    
    TxTypeProviderDefinition(tp)



/// Clones namespaces, type providers, types and members provided by tp, renaming namespace nsp1 into namespace nsp2.
let Desimplify(tp: ISimpleTypeProvider) = 

    //let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    // Unique wrapped type definition objects must be translated to unique wrapper objects, based 
    // on object identity.
    let txTable = TxTable<ISimpleTypeDefinition, Type>()

    let TxCustomAttributesData (inp: seq<CustomAttributeData>) = (Seq.toArray inp :> IList<CustomAttributeData>)

    let rec TxStaticParameter(inp: ISimpleStaticParameter) = 

        { new ParameterInfo() with 

            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.Attributes = (match inp.OptionalValue with None -> ParameterAttributes.None | Some _v -> ParameterAttributes.Optional)
            override __.RawDefaultValue = (match inp.OptionalValue with None -> null | Some v -> v)

            override __.GetCustomAttributesData() = inp.CustomAttributes  |> TxCustomAttributesData

            override __.ToString() = sprintf "provided static parameter %s" inp.Name

            override __.Position                                      = notRequired "Position" 
            override __.GetCustomAttributes(inherited)                = notRequired "GetCustomAttributes" 
            override __.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" 
        }


    and TxParameterDefinition(inp : ISimpleParameter) = 
        { new ParameterInfo() with 

            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.Attributes = 
                (match inp.OptionalValue with None -> ParameterAttributes.None | Some _v -> ParameterAttributes.Optional) |||
                (if inp.IsOut then ParameterAttributes.Out else ParameterAttributes.None)

            override __.RawDefaultValue = (match inp.OptionalValue with None -> null | Some v -> v)

            override __.GetCustomAttributesData() = inp.CustomAttributes  |> TxCustomAttributesData

            override __.ToString() = sprintf "parameter %s" inp.Name

        }
 
    and hashParameterTypes (ps: ISimpleParameter[]) = 
       // This hash code doesn't need to be very good as hashing by name is sufficient to give decent hash granularity
       ps.Length 

    and eqParameterTypes (ps1: ISimpleParameter[]) (ps2: ISimpleParameter[]) = 
        (ps1.Length = ps2.Length) &&
        (ps1,ps2) ||> Array.forall2 (fun p1 p2 -> eqSimpleType p1.ParameterType p2.ParameterType)

    and eqType (ty1: Type) (ty2: Type) = System.Object.ReferenceEquals (ty1, ty2) 
    and eqSimpleTypeDef (ty1: ISimpleTypeDefinition) (ty2: ISimpleTypeDefinition) = System.Object.ReferenceEquals (ty1, ty2) 

    and eqSimpleTypes (tys1: ISimpleType[]) (tys2: ISimpleType[]) = 
        (tys1.Length = tys2.Length) &&
        (tys1,tys2) ||> Array.forall2 eqSimpleType

    and eqSimpleType (ty1: ISimpleType) (ty2: ISimpleType) = 
        match ty1, ty2 with 
        | ISimpleType.TyApp(SimpleTyDef tdef1, args1), ISimpleType.TyApp(SimpleTyDef tdef2, args2) ->
            eqSimpleTypeDef tdef1 tdef2 && eqSimpleTypes args1 args2
        | ISimpleType.TyArray(rank1, arg1), ISimpleType.TyArray(rank2, arg2) ->
            rank1 = rank2 && eqSimpleType arg1 arg2
        | ISimpleType.TyPointer(arg1), ISimpleType.TyPointer(arg2) ->
            eqSimpleType arg1 arg2
        | ISimpleType.TyByRef(arg1), ISimpleType.TyByRef(arg2) ->
            eqSimpleType arg1 arg2
        | _ -> false

    and TxConstructorDefinition declTy (inp: ISimpleConstructor) = 
        { new ConstructorInfo() with

            override __.Name = ".ctor"
            override __.Attributes = MethodAttributes.Public ||| MethodAttributes.RTSpecialName // see ProvidedTypes.fs, which always returns this value
            override __.DeclaringType = declTy
            override __.GetParameters() = inp.Parameters |> Array.map TxParameterDefinition

            override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

            override __.GetHashCode() = hashParameterTypes inp.Parameters
            override __.Equals(that:obj) = 
                match that with 
                | :? ConstructorInfo -> 
                    match tryUnwrap<ISimpleConstructor> that with 
                    | Some thatSC -> eqParameterTypes inp.Parameters thatSC.Parameters
                    | None -> false
                | _ -> false

            override __.ToString() = sprintf "provided constructor(...) in type %s" declTy.Name

            override __.IsDefined(attributeType, inherited)                       = notRequired "IsDefined" 
            override __.Invoke(invokeAttr, binder, parameters, culture)           = notRequired "Invoke"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture)      = notRequired "Invoke"
            override __.ReflectedType                                             = notRequired "ReflectedType"
            override __.GetMethodImplementationFlags()                            = notRequired "GetMethodImplementationFlags"
            override __.MethodHandle                                              = notRequired "MethodHandle"
            override __.GetCustomAttributes(inherited)                            = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)             = notRequired "GetCustomAttributes"

          interface IWraps<ISimpleConstructor> with 
              member x.Value = inp
        }

    and TxMethodDefinition declTy (inp: ISimpleMethod) =
        { new MethodInfo() with 

            override __.GetParameters()   = inp.Parameters  |> Array.map TxParameterDefinition
            override __.Attributes        = 
                (if inp.IsStatic then MethodAttributes.Static else enum 0) |||
                MethodAttributes.Public
            override __.Name              = inp.Name  
            override __.DeclaringType     = declTy
            override __.MemberType        = MemberTypes.Method
            override __.CallingConvention = CallingConventions.HasThis ||| CallingConventions.Standard // Provided types report this by default
            override __.ReturnType        = inp.ReturnType |> TxType

            override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

            override __.GetHashCode() = hash inp.Name + hashParameterTypes inp.Parameters
            override this.Equals(that:obj) = 
                match that with 
                | :? MethodInfo as thatMI -> 
                    inp.Name = thatMI.Name &&
                    eqType this.DeclaringType thatMI.DeclaringType &&
                    match tryUnwrap<ISimpleMethod> that with 
                    | Some thatSM -> eqParameterTypes inp.Parameters thatSM.Parameters 
                    | None -> false
                | _ -> false

            override __.ToString() = sprintf "provided method %s(...) in type %s" inp.Name declTy.Name
    
            override __.MetadataToken = notRequired "MetadataToken"
            override __.MethodHandle = notRequired "MethodHandle"
            override __.ReturnParameter   = notRequired "ReturnParameter" // Note, if necessary we can return "null" here
            override __.IsDefined(attributeType, inherited)                   = notRequired "IsDefined"
            override __.ReturnTypeCustomAttributes                            = notRequired "ReturnTypeCustomAttributes"
            override __.GetBaseDefinition()                                   = notRequired "GetBaseDefinition"
            override __.GetMethodImplementationFlags()                        = notRequired "GetMethodImplementationFlags"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture)  = notRequired "Invoke"
            override __.ReflectedType                                         = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                        = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)         = notRequired "GetCustomAttributes"

          interface IWraps<ISimpleMethod> with 
              member x.Value = inp
        }


    and TxAssociatedMethodDefinition (nm, isStatic, parameters, returnTy) declTy (inp: ISimpleAssociatedMethod) =
        { new MethodInfo() with 

            override __.GetParameters()   = parameters  |> Array.map TxParameterDefinition
            override __.Attributes        = 
                (if isStatic then MethodAttributes.Static else enum 0) |||
                MethodAttributes.Public
            override __.Name              = nm
            override __.DeclaringType     = declTy
            override __.MemberType        = MemberTypes.Method
            override __.CallingConvention = CallingConventions.HasThis ||| CallingConventions.Standard // Provided types report this by default
            override __.ReturnType        = returnTy |> TxType

            override __.GetCustomAttributesData() = [| |] |> TxCustomAttributesData

            override __.GetHashCode() = hash nm + hashParameterTypes parameters
            override this.Equals(that:obj) = 
                match that with 
                | :? MethodInfo as thatMI -> 
                    nm = thatMI.Name &&
                    eqType this.DeclaringType thatMI.DeclaringType &&
                    match tryUnwrap<ISimpleParameter[]> that with 
                    | Some thatParameters -> eqParameterTypes parameters thatParameters 
                    | None -> false
                | _ -> false

            override __.ToString() = sprintf "provided method %s(...) in type %s" nm declTy.Name
    
            override __.MetadataToken = notRequired "MetadataToken"
            override __.MethodHandle = notRequired "MethodHandle"
            override __.ReturnParameter   = notRequired "ReturnParameter" // Note, if necessary we can return "null" here
            override __.IsDefined(attributeType, inherited)                   = notRequired "IsDefined"
            override __.ReturnTypeCustomAttributes                            = notRequired "ReturnTypeCustomAttributes"
            override __.GetBaseDefinition()                                   = notRequired "GetBaseDefinition"
            override __.GetMethodImplementationFlags()                        = notRequired "GetMethodImplementationFlags"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture)  = notRequired "Invoke"
            override __.ReflectedType                                         = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                        = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)         = notRequired "GetCustomAttributes"

          // NOTE: See GetInvokerExpression, which probes for this interface
          interface IWraps<ISimpleAssociatedMethod * ISimpleParameter[]> with 
              member x.Value = (inp, parameters)
        }


    and MakeAssociatedParam (nm, ty) = 
        { new ISimpleParameter with
            override __.Name = nm
            override __.ParameterType = ty
            override __.OptionalValue = None
            override __.IsOut = false
            override __.CustomAttributes = Seq.empty }

    and MakeVoidTy() = TyApp(OtherTyDef(typeof<System.Void>), [| |])


    and TxPropertyGet declTy (inp: ISimpleProperty) (m: ISimpleAssociatedMethod) = 
        m |> TxAssociatedMethodDefinition ("get_" + inp.Name, inp.IsStatic, inp.IndexParameters, inp.PropertyType) declTy


    and TxPropertySet declTy (inp: ISimpleProperty) (m: ISimpleAssociatedMethod) = 
        m |> TxAssociatedMethodDefinition ("set_" + inp.Name, inp.IsStatic, 
                                           [| yield! inp.IndexParameters; yield MakeAssociatedParam ("value", inp.PropertyType) |], 
                                           MakeVoidTy()) declTy

    and TxPropertyDefinition declTy (inp: ISimpleProperty) = 
        { new PropertyInfo() with 

            override __.Name = inp.Name
            override __.Attributes = PropertyAttributes.None
            override __.DeclaringType = declTy
            override __.MemberType = MemberTypes.Property

            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetGetMethod(_nonPublicUnused)= 
                inp.GetMethod 
                |> Option.map (TxPropertyGet declTy inp)
                |> optionToNull

            override __.GetSetMethod(_nonPublicUnused) = 
                inp.SetMethod 
                |> Option.map (TxPropertySet declTy inp)
                |> optionToNull

            override __.GetIndexParameters() = inp.IndexParameters  |> Array.map TxParameterDefinition

            override __.CanRead = inp.GetMethod.IsSome
            override __.CanWrite = inp.SetMethod.IsSome

            override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

            override this.GetHashCode() = hash inp.Name
            override this.Equals(that:obj) = 
                match that with 
                | :? PropertyInfo as thatPI -> 
                    inp.Name = thatPI.Name  &&
                    eqType this.DeclaringType thatPI.DeclaringType 
                | _ -> false

            override __.ToString() = sprintf "provided property %s(...) in type %s" inp.Name declTy.Name

            override __.GetValue(obj, invokeAttr, binder, index, culture)         = notRequired "GetValue"
            override __.SetValue(obj, _value, invokeAttr, binder, index, culture) = notRequired "SetValue"
            override __.GetAccessors(nonPublic)                                   = notRequired "GetAccessors"
            override __.ReflectedType                                             = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                            = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)             = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)                       = notRequired "IsDefined"

          interface IWraps<ISimpleProperty> with 
              member x.Value = inp
        }


    and TxEventAdd declTy (inp: ISimpleEvent) = 
        inp.AddMethod
        |> TxAssociatedMethodDefinition ("add_" + inp.Name, inp.IsStatic, [| MakeAssociatedParam("handler", inp.EventHandlerType) |], MakeVoidTy()) declTy 

    and TxEventRemove declTy (inp: ISimpleEvent) = 
        inp.AddMethod
        |> TxAssociatedMethodDefinition ("remove_" + inp.Name, inp.IsStatic, [| MakeAssociatedParam("handler", inp.EventHandlerType) |], MakeVoidTy()) declTy 

    and TxEventDefinition declTy (inp: ISimpleEvent) = 
        { new EventInfo() with 

            override __.Name = inp.Name 
            override __.Attributes = EventAttributes.None 
            override __.MemberType = MemberTypes.Event
            override __.DeclaringType = declTy
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.GetAddMethod(_nonPublicUnused) = TxEventAdd declTy inp
            override __.GetRemoveMethod(_nonPublicUnused) = TxEventRemove declTy inp
            override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

            override __.GetHashCode() = hash inp.Name
            override this.Equals(that:obj) = 
                match that with 
                | :? EventInfo as thatEI -> 
                    inp.Name = thatEI.Name  &&
                    eqType this.DeclaringType thatEI.DeclaringType 
                | _ -> false

            override __.ToString() = sprintf "provided event %s(...) in type %s" inp.Name declTy.Name

            override __.GetRaiseMethod(nonPublic)                      = notRequired "GetRaiseMethod"
            override __.ReflectedType                                  = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                 = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)  = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)            = notRequired "IsDefined"

          interface IWraps<ISimpleEvent> with 
              member x.Value = inp
        }

    and TxFieldDefinition declTy (inp: ISimpleLiteralField) = 
        { new FieldInfo() with 

            override __.Name = inp.Name 
            override __.Attributes = FieldAttributes.Static ||| FieldAttributes.Literal ||| FieldAttributes.Public 
            override __.MemberType = MemberTypes.Field 
            override __.DeclaringType = declTy

            override __.FieldType = inp.FieldType |> TxType
            override __.GetRawConstantValue()  = inp.LiteralValue 

            override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

            override __.GetHashCode() = hash inp.Name
            override this.Equals(that:obj) = 
                match that with 
                | :? EventInfo as thatFI -> 
                    inp.Name = thatFI.Name  &&
                    eqType this.DeclaringType thatFI.DeclaringType 
                | _ -> false

            override __.ToString() = sprintf "provided literal field %s(...) in type %s" inp.Name declTy.Name
    
            override __.ReflectedType                                          = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                         = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)          = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)                    = notRequired "IsDefined"
            override __.SetValue(obj, _value, invokeAttr, binder, culture) = notRequired "SetValue"
            override __.GetValue(obj)                                         = notRequired "GetValue"
            override __.FieldHandle                                            = notRequired "FieldHandle"

          interface IWraps<ISimpleLiteralField> with 
              member x.Value = inp
        }

    and TxType(ty: ISimpleType) = 
      
        match ty with 
        | ISimpleType.TyApp(tdef, args) ->
            let tdefR = 
               match tdef with 
               | SimpleTyDef tdef -> TxTypeDefinition tdef
               | OtherTyDef ty -> ty
            match args with 
            | [| |] -> tdefR
            | _ -> 
                let argsR = Array.map TxType args
                ProvidedSymbolType(Generic tdefR, Array.toList argsR)  :> Type
        | ISimpleType.TyArray(rank, arg) ->
            let argR = TxType arg
            if rank = 1 then ProvidedSymbolType(SDArray,[argR]) :> Type
            else ProvidedSymbolType(Array rank,[argR]) :> Type
        | ISimpleType.TyPointer(arg) ->
            ProvidedSymbolType(Pointer,[TxType arg]) :> Type
        | ISimpleType.TyByRef(arg) ->
            ProvidedSymbolType(ByRef,[TxType arg]) :> Type

    and TxTypeDefinition (inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->

            let isNested() = Option.isSome inp.DeclaringType
            let adjustTypeAttributes attributes = 
                let visibilityAttributes = 
                    match attributes &&& TypeAttributes.VisibilityMask with 
                    | TypeAttributes.Public when isNested() -> TypeAttributes.NestedPublic
                    | TypeAttributes.NotPublic when isNested() -> TypeAttributes.NestedAssembly
                    | TypeAttributes.NestedPublic when not (isNested()) -> TypeAttributes.Public
                    | TypeAttributes.NestedAssembly 
                    | TypeAttributes.NestedPrivate 
                    | TypeAttributes.NestedFamORAssem
                    | TypeAttributes.NestedFamily
                    | TypeAttributes.NestedFamANDAssem when not (isNested()) -> TypeAttributes.NotPublic
                    | a -> a
                (attributes &&& ~~~TypeAttributes.VisibilityMask) ||| visibilityAttributes

            let fullName = 
              lazy
                let simpleName = inp.Name
                match inp.DeclaringType with
                | Some declaringType -> (TxTypeDefinition declaringType).FullName + "+" + simpleName
                | None ->  
                    match inp.Namespace with
                    | None -> simpleName
                    | Some namespaceName -> namespaceName + "." + simpleName

            { new Type() with 
                override __.Name = inp.Name 
                override __.Assembly = inp.Assembly 
                override __.FullName = fullName.Value
                    
                override __.Namespace = inp.Namespace |> optionToNull
                override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition |> optionToNull
                override __.MemberType = if isNested() then MemberTypes.NestedType else MemberTypes.TypeInfo

                override __.BaseType = inp.BaseType |> Option.map TxType |> optionToNull
                override __.GetInterfaces() = inp.Interfaces |> Array.map TxType

                override this.GetConstructors(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Constructors |> Array.map (TxConstructorDefinition this)

                override this.GetMethods(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    [| for m in inp.Methods do
                           yield m |> TxMethodDefinition this

                       for p in inp.Properties do 
                          match p.GetMethod with 
                          | Some m -> yield TxPropertyGet this p m
                          | None -> ()
                          match p.SetMethod with 
                          | Some m -> yield TxPropertySet this p m
                          | None -> () 

                       for p in inp.Events do 
                          yield TxEventAdd this p 
                          yield TxEventRemove this p 
                    |]


                override this.GetField(name, bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Fields
                    |> Array.tryPick (fun p -> if p.Name = name then Some (TxFieldDefinition this p) else None) 
                    |> optionToNull
                    //inp.GetField(name) |> Option.map (TxFieldDefinition this) |> optionToNull

                override this.GetFields(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Fields |> Array.map (TxFieldDefinition this)

                override this.GetEvent(name, bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Events
                    |> Array.tryPick (fun p -> if p.Name = name then Some (TxEventDefinition this p) else None) 
                    |> optionToNull

                    //inp.GetEvent(name) |> Option.map (TxEventDefinition this) |> optionToNull

                override this.GetEvents(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Events |> Array.map (TxEventDefinition this)

                override this.GetProperties(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Properties |> Array.map (TxPropertyDefinition this)

                override this.GetMembers(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    [| for x in this.GetMethods() do yield (x :> MemberInfo)
                       for x in this.GetFields() do yield (x :> MemberInfo)
                       for x in this.GetProperties() do yield (x :> MemberInfo)
                       for x in this.GetEvents() do yield (x :> MemberInfo)
                       for x in this.GetNestedTypes() do yield (x :> MemberInfo) |]
 
                override this.GetNestedTypes(bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.NestedTypes |> Array.map TxTypeDefinition
 
                override this.GetNestedType(name, bindingFlagsUnused) = 
                    assert (bindingFlagsUnused = bindingFlagsWithoutDeclaredOnly || bindingFlagsUnused = bindingFlags)  
                    let declaredOnly = bindingFlagsUnused.HasFlag(BindingFlags.DeclaredOnly)
                    inp.GetNestedType(name, declaredOnly) |> Option.map TxTypeDefinition |> optionToNull

                override this.GetPropertyImpl(name, bindingFlagsUnused, binderUnused, returnTypeUnused, typesUnused, modifiersUnused) = 
                    assert (bindingFlagsUnused = bindingFlags)
                    inp.Properties 
                    |> Array.tryPick (fun p -> if p.Name = name then Some (TxPropertyDefinition this p) else None) 
                    |> optionToNull
        
                // Every implementation of System.Type must meaningfully implement these
                override this.MakeGenericType(args) = ProvidedSymbolType(SymbolKind.Generic this, Array.toList args) :> Type
                override this.MakeArrayType() = ProvidedSymbolType(SymbolKind.SDArray, [this]) :> Type
                override this.MakeArrayType arg = ProvidedSymbolType(SymbolKind.Array arg, [this]) :> Type
                override this.MakePointerType() = ProvidedSymbolType(SymbolKind.Pointer, [this]) :> Type
                override this.MakeByRefType() = ProvidedSymbolType(SymbolKind.ByRef, [this]) :> Type

                override __.GetAttributeFlagsImpl() = 
                    TypeAttributes.Public ||| 
                    TypeAttributes.Class ||| 
                    TypeAttributes.Sealed |||
                    enum (int32 TypeProviderTypeAttributes.IsErased)

                override __.IsArrayImpl() = false
                override __.IsByRefImpl() = false
                override __.IsPointerImpl() = false
                override __.IsPrimitiveImpl() = false
                override __.IsCOMObjectImpl() = false
                override __.IsGenericType = false
                override __.IsGenericTypeDefinition = false

                override __.HasElementTypeImpl() = false

                override this.UnderlyingSystemType = typeof<System.Type> // Same as ProvidedTypes, never gets called
                //override __.UnderlyingSystemType = inp.UnderlyingSystemType |> TxTypeSymbol
                override __.GetCustomAttributesData() = inp.CustomAttributes |> TxCustomAttributesData

                override this.Equals(that:obj) = System.Object.ReferenceEquals (this, that) 

                override __.ToString() = sprintf "provided type %s (full name %s)" inp.Name fullName.Value

                override __.GetGenericArguments() = notRequired "GetGenericArguments"
                override __.GetGenericTypeDefinition() = notRequired "GetGenericTypeDefinition"
                override __.GetMember(name,mt,bindingFlagsUnused)                                                      = notRequired "GUID"
                override __.GUID                                                                                      = notRequired "GUID"
                override __.GetMethodImpl(name, bindingFlagsUnused, binder, callConvention, types, modifiers)          = notRequired "GetMethodImpl"
                override __.GetConstructorImpl(bindingFlagsUnused, binder, callConvention, types, modifiers)           = notRequired "GetConstructorImpl"
                override __.GetCustomAttributes(inherited)                                                            = notRequired "GetCustomAttributes"
                override __.GetCustomAttributes(attributeType, inherited)                                             = notRequired "GetCustomAttributes"
                override __.IsDefined(attributeType, inherited)                                                       = notRequired "IsDefined"
                override __.GetInterface(name, ignoreCase)                                                            = notRequired "GetInterface"
                override __.Module                                                                                    = notRequired "Module" : Module 
                override __.GetElementType()                                                                          = notRequired "GetElementType"
                override __.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters) = notRequired "InvokeMember"
                override __.AssemblyQualifiedName                                                                     = notRequired "AssemblyQualifiedName"

              interface IWraps<ISimpleTypeDefinition> with 
                  member x.Value = inp
            }


    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new IProvidedNamespace with
            override __.GetNestedNamespaces() = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName 
            override __.GetTypes() = inp.TypeDefinitions |> Array.map TxTypeDefinition
            override __.ResolveTypeName(typeName) =  inp.GetTypeDefinition(typeName) |> Option.map TxTypeDefinition |> optionToNull
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ISimpleTypeProvider) = 
        let invalidateE = new Event<EventHandler,EventArgs>()    


        let self = 
          { new ITypeProvider with 
            override __.GetNamespaces() = inp.Namespaces |> Array.map TxNamespaceDefinition

            override __.GetInvokerExpression(syntheticMethodBase, parameters) = 
                    let getImpl = 
                        match box syntheticMethodBase with 

                        // NOTE: see the hidden interface inserted by TxMethodDefinition
                        | :? IWraps<ISimpleMethod> as x -> x.Value.GetImplementation

                        // NOTE: see the hidden interface inserted by TxConstructorDefinition
                        | :? IWraps<ISimpleConstructor> as x -> x.Value.GetImplementation

                        // NOTE: see the hidden interface inserted by TxAssociatedMethodDefinition
                        | :? IWraps<ISimpleAssociatedMethod * ISimpleParameter[]> as x -> (fst x.Value).GetImplementation
                        | _ -> failwith "unexpected - why are we being asked about this method?"


                    // TBD
                    //System.Diagnostics.Debugger.Break()

                    let parameterVars2 = 
                        [| for p in parameters do 
                                match p with 
                                | Quotations.Patterns.Var(v) ->  yield Quotations.Var(v.Name, (* computeReprType *) v.Type)
                                | _ -> failwith "unexpected non-var" |]

                    let q2 = getImpl(parameterVars2)
                    let tab = Map.ofSeq (Array.zip parameterVars2 parameters)
                    let q = q2.Substitute (tab.TryFind)
                    q

            override __.GetStaticParameters(typeWithoutArguments) = 
                unwrapOther<ISimpleTypeDefinition>(typeWithoutArguments).StaticParameters |> Array.map TxStaticParameter

            override __.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) = 
                let inpApplied = unwrapOther<ISimpleTypeDefinition>(typeWithoutArguments).ApplyStaticArguments(typePathWithArguments, objs) 
                let inpWrapped = inpApplied |> TxTypeDefinition
                inpWrapped

            override __.GetGeneratedAssemblyContents(assembly) = 
                //System.Diagnostics.Debugger.Break()
                null // should not be called

            [<CLIEvent>]
            override __.Invalidate = invalidateE.Publish

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
        do inp.Invalidate.Add (fun () -> invalidateE.Trigger(self,EventArgs()) )
        self
    
    TxTypeProviderDefinition(tp)

(*

    and TxMethodSymbol(inp: MethodInfo) =
        if inp = null then null else
        { new MethodInfo() with 

            override __.Name = inp.Name 
            override __.Attributes = inp.Attributes 
            override __.MemberType = inp.MemberType 

            override __.IsGenericMethod =  inp.IsGenericMethod 
            override __.GetGenericArguments() = inp.GetGenericArguments() |> Array.map TxTypeSymbol
            override __.GetGenericMethodDefinition() = inp.GetGenericMethodDefinition() |> TxMethodDefinition
            override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
            override __.MetadataToken = inp.MetadataToken 
            override __.CallingConvention = inp.CallingConvention 

            override __.ReturnType = inp.ReturnType |> TxTypeSymbol
            override __.GetParameters() = inp.GetParameters() |> Array.map TxParameterDefinition
            override __.ReturnParameter = inp.ReturnParameter |> TxParameterDefinition

            override __.GetHashCode() = inp.GetHashCode()  
            override __.Equals(that:obj) = inp.Equals(unwrapObj<MethodInfo> that) 
            override __.ToString() = inp.ToString() 

            override __.IsDefined(attributeType, inherited)                       = notRequired "IsDefined"
            override __.ReturnTypeCustomAttributes                                = notRequired "ReturnTypeCustomAttributes"
            override __.GetBaseDefinition()                                       = notRequired "GetBaseDefinition"
            override __.GetMethodImplementationFlags()                            = notRequired "GetMethodImplementationFlags"
            override __.MethodHandle                                              = notRequired "MethodHandle"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture) = notRequired "Invoke"
            override __.ReflectedType                                             = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                            = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)             =  notRequired "GetCustomAttributes" 

          interface IWraps<MethodInfo> with 
              member x.Value = inp
       }

*)




