module FSharp.ProvidedTypes.GeneralCombinators

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices

[<AutoOpen>]
module internal Utils = 
    let notRequired msg = failwith ("not required: " + msg)
    let NIX x = x   // Inner --> Outer
    let XIN x = x   // Outer --> Inner

    /// Indicates that an object is a simple wrapper for another object of the indicated type,
    /// used for implementing equality in terms of the underlying wrapped objects.
    type IWraps<'T> =
         abstract Value : 'T

    let tryUnwrap<'T> (x:obj) = 
        match x with 
        | :? IWraps<'T> as t -> Some t.Value
        | _ -> None

    let unwrapOther<'T> (x:obj) = 
        match x with 
        | :? IWraps<'T> as t -> t.Value
        | _ -> failwith "unexpected unwrap failure"

    let unwrapObj<'T> (x:obj) = 
        match x with 
        | :? IWraps<'T> as t -> box t.Value
        | _ -> x

    let unwrap<'T> (x:'T) = 
        match box x with 
        | :? IWraps<'T> as t -> t.Value
        | _ -> x

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    // Unique wrapped type definition objects must be translated to unique wrapper objects, based 
    // on object identity.
    type TxTable<'T1, 'T2 when 'T1 : not struct>() = 
        let tab = Dictionary<'T1, 'T2>(HashIdentity.Reference)
        member __.Get inp f = 
            if tab.ContainsKey inp then 
                tab.[inp] 
            else 
                let res = f() 
                tab.[inp] <- res
                res

type System.String with 
    member s.ReplacePrefix (s1:string, s2:string) =  
        if s.StartsWith(s1) then s2 + s.[s1.Length..] else s


/// A base type suitable for implementing a type provider component based on a computed ITypeProvider instance.
type TypeProviderExpression (inp: ITypeProvider) = 

    interface ITypeProvider with 
        override __.GetNamespaces() = inp.GetNamespaces() 
        override __.GetInvokerExpression(methodBase, parametersUsingReprTypes) = inp.GetInvokerExpression(methodBase, parametersUsingReprTypes) 
        override __.GetStaticParameters(typeWithoutArguments) = inp.GetStaticParameters(typeWithoutArguments) 
        override __.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) = inp.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) 
        override __.GetGeneratedAssemblyContents(assembly) = inp.GetGeneratedAssemblyContents(assembly)
        [<CLIEvent>]
        override __.Invalidate = inp.Invalidate 

    interface System.IDisposable with 
        override x.Dispose() = inp.Dispose()
                                                                        
/// Create a TypeProviderConfig suitable for passing to a slave type provider instance.
///
/// The slave type provider instance expects a configuration with a matching
/// RuntimeAssembly value. The other configuration parameters can remain unchanged.
let ConfigForOtherTypeProvider(config: TypeProviderConfig, runtimeAssembly) = 
    TypeProviderConfig(config.SystemRuntimeContainsType,
                       ResolutionFolder=config.ResolutionFolder,
                       RuntimeAssembly=runtimeAssembly,
                       ReferencedAssemblies=config.ReferencedAssemblies,
                       TemporaryFolder=config.TemporaryFolder,
                       IsInvalidationSupported=config.IsInvalidationSupported,
                       IsHostedExecution=config.IsHostedExecution,
                       SystemRuntimeAssemblyVersion=config.SystemRuntimeAssemblyVersion)




/// Represents the type constructor in a provided symbol type.
type SymbolKind = 
    | SDArray 
    | Array of int 
    | Pointer 
    | ByRef 
    | Generic of System.Type 
    | FSharpTypeAbbreviation of (System.Reflection.Assembly * string * string[])


/// Represents an array or other symbolic type involving a provided type as the argument.
/// See the type provider spec for the methods that must be implemented.
/// Note that the type provider specification does not require us to implement pointer-equality for provided types.
type internal ProvidedSymbolType(kind: SymbolKind, args: Type list) =
    inherit Type()

    let notRequired msg = failwith ("not required: " + msg)
    let rec isEquivalentTo (thisTy: Type) (otherTy: Type) =
        match thisTy, otherTy with
        | (:? ProvidedSymbolType as thisTy), (:? ProvidedSymbolType as thatTy) -> (thisTy.Kind,thisTy.Args) = (thatTy.Kind, thatTy.Args)
        | (:? ProvidedSymbolType as thisTy), otherTy | otherTy, (:? ProvidedSymbolType as thisTy) ->
            match thisTy.Kind, thisTy.Args with
            | SymbolKind.SDArray, [ty] | SymbolKind.Array _, [ty] when otherTy.IsArray-> ty.Equals(otherTy.GetElementType())
            | SymbolKind.ByRef, [ty] when otherTy.IsByRef -> ty.Equals(otherTy.GetElementType())
            | SymbolKind.Pointer, [ty] when otherTy.IsPointer -> ty.Equals(otherTy.GetElementType())
            | SymbolKind.Generic baseTy, args -> otherTy.IsGenericType && isEquivalentTo baseTy (otherTy.GetGenericTypeDefinition()) && Seq.forall2 isEquivalentTo args (otherTy.GetGenericArguments())
            | _ -> false
        | a, b -> a.Equals b


    static member convType (parameters: Type list) (ty:Type) = 
        if ty.IsGenericType then 
            let args = Array.map (ProvidedSymbolType.convType parameters) (ty.GetGenericArguments())
            ProvidedSymbolType(Generic (ty.GetGenericTypeDefinition()), Array.toList args)  :> Type
        elif ty.HasElementType then 
            let ety = ProvidedSymbolType.convType parameters (ty.GetElementType()) 
            if ty.IsArray then 
                let rank = ty.GetArrayRank()
                if rank = 1 then ProvidedSymbolType(SDArray,[ety]) :> Type
                else ProvidedSymbolType(Array rank,[ety]) :> Type
            elif ty.IsPointer then ProvidedSymbolType(Pointer,[ety]) :> Type
            elif ty.IsByRef then ProvidedSymbolType(ByRef,[ety]) :> Type
            else ty
        elif ty.IsGenericParameter then 
            if ty.GenericParameterPosition <= parameters.Length - 1 then 
                parameters.[ty.GenericParameterPosition]
            else
                ty
        else ty

    override __.FullName =   
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg.FullName + "[]" 
        | SymbolKind.Array _,[arg] -> arg.FullName + "[*]" 
        | SymbolKind.Pointer,[arg] -> arg.FullName + "*" 
        | SymbolKind.ByRef,[arg] -> arg.FullName + "&"
        | SymbolKind.Generic gty, args -> gty.FullName + "[" + (args |> List.map (fun arg -> arg.ToString()) |> String.concat ",") + "]"
        | SymbolKind.FSharpTypeAbbreviation (_,nsp,path),args -> String.concat "." (Array.append [| nsp |] path) + args.ToString()
        | _ -> failwith "unreachable"
   
    /// Although not strictly required by the type provider specification, this is required when doing basic operations like FullName on
    /// .NET symbolic types made from this type, e.g. when building Nullable<SomeProvidedType[]>.FullName
    override __.DeclaringType =                                                                 
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg
        | SymbolKind.Array _,[arg] -> arg
        | SymbolKind.Pointer,[arg] -> arg
        | SymbolKind.ByRef,[arg] -> arg
        | SymbolKind.Generic gty,_ -> gty
        | SymbolKind.FSharpTypeAbbreviation _,_ -> null
        | _ -> failwith "unreachable"

    override __.IsAssignableFrom(otherTy) = 
        match kind with
        | Generic gtd ->
            if otherTy.IsGenericType then
                let otherGtd = otherTy.GetGenericTypeDefinition()
                let otherArgs = otherTy.GetGenericArguments()
                let yes = gtd.Equals(otherGtd) && Seq.forall2 isEquivalentTo args otherArgs
                yes
            else
                base.IsAssignableFrom(otherTy)
        | _ -> base.IsAssignableFrom(otherTy)

    override __.Name =
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> arg.Name + "[]" 
        | SymbolKind.Array _,[arg] -> arg.Name + "[*]" 
        | SymbolKind.Pointer,[arg] -> arg.Name + "*" 
        | SymbolKind.ByRef,[arg] -> arg.Name + "&"
        | SymbolKind.Generic gty, args -> gty.FullName + args.ToString()
        | SymbolKind.FSharpTypeAbbreviation (_,_,path),_ -> path.[path.Length-1]
        | _ -> failwith "unreachable"

    override __.BaseType =
        match kind with 
        | SymbolKind.SDArray -> typeof<System.Array>
        | SymbolKind.Array _ -> typeof<System.Array>
        | SymbolKind.Pointer -> typeof<System.ValueType>
        | SymbolKind.ByRef -> typeof<System.ValueType>
        | SymbolKind.Generic gty  -> ProvidedSymbolType.convType args gty.BaseType
        | SymbolKind.FSharpTypeAbbreviation _ -> typeof<obj>

    override __.GetArrayRank() = (match kind with SymbolKind.Array n -> n | SymbolKind.SDArray -> 1 | _ -> invalidOp "non-array type")
    override __.IsArrayImpl() = (match kind with SymbolKind.Array _ | SymbolKind.SDArray -> true | _ -> false)
    override __.IsByRefImpl() = (match kind with SymbolKind.ByRef _ -> true | _ -> false)
    override __.IsPointerImpl() = (match kind with SymbolKind.Pointer _ -> true | _ -> false)
    override __.IsPrimitiveImpl() = false
    override __.IsGenericType = (match kind with SymbolKind.Generic _ -> true | _ -> false)
    override __.GetGenericArguments() = (match kind with SymbolKind.Generic _ -> args |> List.toArray | _ -> invalidOp "non-generic type")
    override __.GetGenericTypeDefinition() = (match kind with SymbolKind.Generic e -> e | _ -> invalidOp "non-generic type")
    override __.IsCOMObjectImpl() = false
    override __.HasElementTypeImpl() = (match kind with SymbolKind.Generic _ -> false | _ -> true)
    override __.GetElementType() = (match kind,args with (SymbolKind.Array _  | SymbolKind.SDArray | SymbolKind.ByRef | SymbolKind.Pointer),[e] -> e | _ -> invalidOp "not an array, pointer or byref type")
    override this.ToString() = this.FullName

    override this.Module : Module                                                                  = notRequired "Module" this.Name
    override this.Assembly = 
        match kind with 
        | SymbolKind.FSharpTypeAbbreviation (assembly,_nsp,_path) -> assembly
        | SymbolKind.Generic gty -> gty.Assembly
        | _ -> notRequired "Assembly" this.Name
    override this.Namespace = 
        match kind with 
        | SymbolKind.FSharpTypeAbbreviation (_assembly,nsp,_path) -> nsp
        | _ -> notRequired "Namespace" this.Name

    override this.GetHashCode()                                                                    = 
        match kind,args with 
        | SymbolKind.SDArray,[arg] -> 10 + hash arg
        | SymbolKind.Array _,[arg] -> 163 + hash arg
        | SymbolKind.Pointer,[arg] -> 283 + hash arg
        | SymbolKind.ByRef,[arg] -> 43904 + hash arg
        | SymbolKind.Generic gty,_ -> 9797 + hash gty + List.sumBy hash args
        | SymbolKind.FSharpTypeAbbreviation _,_ -> 3092
        | _ -> failwith "unreachable"
    
    override this.Equals(other: obj) =
        match other with
        | :? ProvidedSymbolType as otherTy -> (kind, args) = (otherTy.Kind, otherTy.Args)
        | _ -> false

    member this.Kind = kind
    member this.Args = args
    
    override this.GetConstructors _bindingAttr                                                      = notRequired "GetConstructors" this.Name
    override this.GetMethodImpl(_name, _bindingAttr, _binderBinder, _callConvention, _types, _modifiers) = 
        match kind with
        | Generic gtd -> 
            let ty = gtd.GetGenericTypeDefinition().MakeGenericType(Array.ofList args)
            ty.GetMethod(_name, _bindingAttr)
        | _ -> notRequired "GetMethodImpl" this.Name
    override this.GetMembers _bindingAttr                                                           = notRequired "GetMembers" this.Name
    override this.GetMethods _bindingAttr                                                           = notRequired "GetMethods" this.Name
    override this.GetField(_name, _bindingAttr)                                                      = notRequired "GetField" this.Name
    override this.GetFields _bindingAttr                                                            = notRequired "GetFields" this.Name
    override this.GetInterface(_name, _ignoreCase)                                                   = notRequired "GetInterface" this.Name
    override this.GetInterfaces()                                                                  = notRequired "GetInterfaces" this.Name
    override this.GetEvent(_name, _bindingAttr)                                                      = notRequired "GetEvent" this.Name
    override this.GetEvents _bindingAttr                                                            = notRequired "GetEvents" this.Name
    override this.GetProperties _bindingAttr =   notRequired "GetProperties" this.Name
    override this.GetPropertyImpl(_name, _bindingAttr, _binder, _returnType, _types, _modifiers)         = notRequired "GetPropertyImpl" this.Name
    override this.GetNestedTypes _bindingAttr                                                       = notRequired "GetNestedTypes" this.Name
    override this.GetNestedType(_name, _bindingAttr)                                                 = notRequired "GetNestedType" this.Name
    override this.GetAttributeFlagsImpl()                                                          = notRequired "GetAttributeFlagsImpl" this.Name
    override this.UnderlyingSystemType                                                             = 
        match kind with 
        | SymbolKind.SDArray
        | SymbolKind.Array _
        | SymbolKind.Pointer
        | SymbolKind.FSharpTypeAbbreviation _
        | SymbolKind.ByRef -> notRequired "UnderlyingSystemType" this.Name
        | SymbolKind.Generic gty -> gty.UnderlyingSystemType      
    override this.GetCustomAttributesData()                                                        =  ([| |] :> IList<_>)
    override this.MemberType                                                                       = notRequired "MemberType" this.Name
    override this.GetMember(_name,_mt,_bindingAttr)                                                = notRequired "GetMember" this.Name
    override this.GUID                                                                             = notRequired "GUID" this.Name
    override this.InvokeMember(_name, _invokeAttr, _binder, _target, _args, _modifiers, _culture, _namedParameters) = notRequired "InvokeMember" this.Name
    override this.AssemblyQualifiedName                                                            = notRequired "AssemblyQualifiedName" this.Name
    override this.GetConstructorImpl(_bindingAttr, _binder, _callConvention, _types, _modifiers)   = notRequired "GetConstructorImpl" this.Name
    override this.GetCustomAttributes(_inherit)                                                    = [| |]
    override this.GetCustomAttributes(_attributeType, _inherit)                                    = [| |]
    override this.IsDefined(_attributeType, _inherit)                                              = false
    // FSharp.Data addition: this was added to support arrays of arrays
    override this.MakeArrayType() = ProvidedSymbolType(SymbolKind.SDArray, [this]) :> Type
    override this.MakeArrayType arg = ProvidedSymbolType(SymbolKind.Array arg, [this]) :> Type
