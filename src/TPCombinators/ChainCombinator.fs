module FSharp.ProvidedTypes.ChainCombinator

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open FSharp.ProvidedTypes.GeneralCombinators

/// Sequentially composes two type providers tp1 & tp2, by transforming the string outputs of tp1 recognized by tp2 to types provided by tp2
let Chain(tp1: ITypeProvider, resolver: (string -> Type option)) = 

// PropertyInfo = name + static type + how to evaluate = (string * Type * how-to-evaluate)



//let Chain(tp1: ITypeProvider, resolver: (PropertyInfo -> (string * Type) list), ?propNameFilter:string) = 
//let Chain(tp1: ITypeProvider, resolver: (PropertyInfo -> PropertyInfo list)) = 
//let Chain(tp1: ITypeProvider, resolver: (string -> Type option)) = 

// Chain(FS, (fun obj -> [ "freebase", FBType ]))
// Chain(DBP, "sameAs", "freebase", (fun stringArray -> [ "freebase", FBType ]))

// MINIMUM NECESSARY FOR FS "*.csv" --> CSV
//
//let Chain(tp1: ITypeProvider, resolver: (string -> (Type * + more ) option) = 

// MINIMUM NECESSARY FOR DB "sameAs" --> FB
//
//let Chain(tp1: ITypeProvider, origPropName:string, producedName:string, staticPart: (obj -> Type * (methodBase * ... -> Expr option))) = 

// For a type provider tp1 that (optionally?) uses the GDC pattern,
// transform all (instance) properties named 'origPropName' to produce a new property 'producedPropName' 
// of type 'SimplifiedType' whose implementation is given by 'getImpl'
//
//let Chain(tp1: ITypeProvider, origPropName:string, producedPropName:string, staticPart: (string[] -> SimplifiedType * getImpl:(Expr[] -> Expr)))) = 
//
//    GetProperties() -> 
//        [ for p in tp1.GetProperties() do 
//             yield p
//             if p = origPropName r then 
//                yield makeProperty (producedName, resolver (eval p))
//        ]
// 
//    GetProperty(reqPropName) -> 
//        if reqPropName = producedName then 
//            makeProperty (producedName, resolver (eval p), 
//                          getImplementation=(fun args -> getImpl(args)   ))
//        else
//            tp1.GetProperty(reqPropName)
//
//    GetInvokerExpression(methodBase,args) -> 
//        match getInvoker(methodBase,args) with 
//        | Some res -> res 
//        | None -> tp1.GetInvokerExpresion(methodBase,args)


    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

//    let dataContextMethod =
//        tp1.GetNamespaces()
//        |> Array.map (fun ns -> ns.GetTypes())
//        |> Array.concat
//        |> Array.map (fun ty -> ty.GetMethods())
//        |> Array.concat
//        |> Array.find (fun m -> m.IsStatic && m.Name = "GetDataContext")
//
//    let expr = tp1.GetInvokerExpression(dataContextMethod, [| |])
//    let dataContextObj = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr)

    let contextObjs = Dictionary<Type, obj>()
//    contextObjs.[dataContextObj.GetType()] <- dataContextObj

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    // Unique wrapped type definition objects must be translated to unique wrapper objects, based 
    // on object identity.
    let txTable = TxTable<Type, Type>()


    // The transformation we perform on the assembly. isTarget indicates if this is a provided object we should transform. 
    //
    // For now we just transform ALL erased objects.  This assumes one set of provided types is closed,
    // i.e. doesn't refer to any other provided types.
    let TxAssembly isTarget (a:Assembly) = if isTarget then thisAssembly else a
    let TxNamespaceName isTarget (ns:string) = ns
    let TxFullTypeName isTarget  (tn:string) = tn

    let TxCustomAttributeData (inp: CustomAttributeData) =  inp
            //{ new CustomAttributeData() with 
            //    member __.Constructor =  typeof<ParamArrayAttribute>.GetConstructors().[0]
            //    member __.ConstructorArguments = upcast [| |]
            //    member __.NamedArguments = upcast [| |] }

    let TxCustomAttributesData (inp: IList<CustomAttributeData>) =
        inp |> Seq.map TxCustomAttributeData |> Seq.toArray :> IList<_>

    let rec TxStaticParameter(inp: ParameterInfo) = 

        { new ParameterInfo() with 

            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.Attributes = inp.Attributes
            override __.RawDefaultValue = inp.RawDefaultValue

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData()  |> TxCustomAttributesData

            override __.ToString() = inp.ToString() |> NIX

            override __.Position                                      = notRequired "Position" 
            override __.GetCustomAttributes(inherited)                = notRequired "GetCustomAttributes" 
            override __.GetCustomAttributes(attributeType, inherited) = notRequired "GetCustomAttributes" 

          interface IWraps<ParameterInfo> with 
              member x.Value = inp
        }


    and TxParameterDefinition(inp : ParameterInfo) = 
        if inp = null then null else
        { new ParameterInfo() with 

            override __.Name = inp.Name |> NIX
            override __.ParameterType = inp.ParameterType |> TxTypeSymbol
            override __.Attributes = inp.Attributes  |> NIX
            override __.RawDefaultValue = inp.RawDefaultValue |> NIX

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData()  |> TxCustomAttributesData

            override __.ToString() = inp.ToString() |> NIX

          interface IWraps<ParameterInfo> with 
              member x.Value = inp
        }
 
    and TxConstructorDefinition(inp: ConstructorInfo) = 
        if inp = null then null else
        { new ConstructorInfo() with

            override __.Name = inp.Name |> NIX
            override __.Attributes = inp.Attributes |> NIX
            override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
            override __.GetParameters() = inp.GetParameters() |> Array.map TxParameterDefinition

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

            override __.GetHashCode() = inp.GetHashCode()  |> NIX
            override __.Equals(that:obj) = inp.Equals(unwrapObj<ConstructorInfo> that) 
            override __.ToString() = inp.ToString() |> NIX

            override __.IsDefined(attributeType, inherited)                       = notRequired "IsDefined" 
            override __.Invoke(invokeAttr, binder, parameters, culture)           = notRequired "Invoke"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture)     = notRequired "Invoke"
            override __.ReflectedType                                             = notRequired "ReflectedType"
            override __.GetMethodImplementationFlags()                            = notRequired "GetMethodImplementationFlags"
            override __.MethodHandle                                              = notRequired "MethodHandle"
            override __.GetCustomAttributes(inherited)                            = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)             = notRequired "GetCustomAttributes"

          interface IWraps<ConstructorInfo> with 
              member x.Value = inp
        }

    and TxMethodDefinition(inp: MethodInfo) =
        if inp = null then null else
        { new MethodInfo() with 

            override __.GetParameters()   = inp.GetParameters()  |> Array.map TxParameterDefinition
            override __.Attributes        = inp.Attributes |> NIX
            override __.Name              = inp.Name  |> NIX
            override __.DeclaringType     = inp.DeclaringType  |> TxTypeDefinition
            override __.MemberType        = inp.MemberType  |> NIX
            override __.CallingConvention = inp.CallingConvention  |> NIX
            override __.ReturnType        = inp.ReturnType |> TxTypeSymbol
            override __.ReturnParameter   = inp.ReturnParameter  |> TxParameterDefinition

            // These don't have to return fully accurate results - they are used 
            // by the F# Quotations library function SpecificCall as a pre-optimization
            // when comparing methods
            override __.MetadataToken = inp.MetadataToken |> NIX
            override __.MethodHandle = inp.MethodHandle |> NIX

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

            override __.GetHashCode() = inp.GetHashCode()  |> NIX
            override __.Equals(that:obj) = inp.Equals(unwrapObj<MethodInfo> that) 
            override __.ToString() = inp.ToString() |> NIX
    
            override __.IsDefined(attributeType, inherited)                   = notRequired "IsDefined"
            override __.ReturnTypeCustomAttributes                            = notRequired "ReturnTypeCustomAttributes"
            override __.GetBaseDefinition()                                   = notRequired "GetBaseDefinition"
            override __.GetMethodImplementationFlags()                        = notRequired "GetMethodImplementationFlags"
            override __.Invoke(obj, invokeAttr, binder, parameters, culture)  = notRequired "Invoke"
            override __.ReflectedType                                         = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                        = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)         = notRequired "GetCustomAttributes"

          interface IWraps<MethodInfo> with 
              member x.Value = inp
        }

    and TxPropertyDefinition(inp:PropertyInfo) = 
        if inp = null then null else
        { new PropertyInfo() with 

            override __.Name = inp.Name |> NIX
            override __.Attributes = inp.Attributes |> NIX
            override __.DeclaringType = inp.DeclaringType |> TxTypeSymbol
            override __.MemberType = inp.MemberType |> NIX

            override __.PropertyType = 
//                    let thisObj = contextObjs.[__.DeclaringType]
//                let expr = Expr.PropertyGet(Expr.Value(thisObj), inp)
//                let originalResult = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr)
//                if inp.PropertyType = typeof<string> then
//                    // tp1 return type is string, so see if tp2 recognizes it
//                    match tp2.GetTypeById(originalResult :?> string) with
//                    | Some ty -> ty
//                    | None -> inp.PropertyType |> TxTypeSymbol
//                else
//                    // if object returned is a provided object (i.e. type starts with provided namespace) then remember it
//                    if inp.PropertyType.Namespace.StartsWith(tp1.GetNamespaces().[0].NamespaceName) then 
//                        contextObjs.[originalResult.GetType()] <- originalResult
                    inp.PropertyType |> TxTypeSymbol

            override __.GetGetMethod(_nonPublicUnused) = 
            (*
                let ctxtObj = contextObjs.[__.DeclaringType.DeclaringType]
                let getterMethod = inp.GetGetMethod()

                // GetInvokerExpression has a whacky contract. Here's what we know
                //   (a) it needs variables
                //   (b) the type of the 'this' variable must be the runtime (representation) type of the 'this' parameter
                let thisVarStaticType = getterMethod.DeclaringType
                let thisVarRuntimeType = thisVarStaticType.BaseType  // this is an approximation - we should compute the representation type by walking the BaseType chain to the first non-provided type

                let thisVar = Var("this",thisVarRuntimeType)
                let expr = tp1.GetInvokerExpression(getterMethod, [| Expr.Var(thisVar) |])
                let expr2 = expr.Substitute( fun v -> if v = thisVar then Some(Expr.Value(ctxtObj,thisVarRuntimeType)) else None)
                let originalResult = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr2)
                *)
                inp.GetGetMethod(_nonPublicUnused) |> TxMethodDefinition
                
            override __.GetSetMethod(_nonPublicUnused) = inp.GetSetMethod(_nonPublicUnused) |> TxMethodDefinition
            override __.GetIndexParameters() = inp.GetIndexParameters()  |> Array.map TxParameterDefinition

            override __.CanRead = inp.CanRead |> NIX
            override __.CanWrite = inp.CanWrite |> NIX

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

            override __.GetHashCode() = inp.GetHashCode()  |> NIX
            override __.Equals(that:obj) = inp.Equals(unwrapObj<PropertyInfo> that) 
            override __.ToString() = inp.ToString() |> NIX

            override __.GetValue(obj, invokeAttr, binder, index, culture)         = notRequired "GetValue"
            override __.SetValue(obj, _value, invokeAttr, binder, index, culture) = notRequired "SetValue"
            override __.GetAccessors(nonPublic)                                   = notRequired "GetAccessors"
            override __.ReflectedType                                             = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                            = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)             = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)                       = notRequired "IsDefined"

          interface IWraps<PropertyInfo> with 
              member x.Value = inp
        }

    and TxEventDefinition(inp: EventInfo) = 
        if inp = null then null else
        { new EventInfo() with 

            override __.Name = inp.Name |> NIX
            override __.Attributes = inp.Attributes |> NIX
            override __.MemberType = inp.MemberType |> NIX
            override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition

            override __.EventHandlerType = inp.EventHandlerType |> TxTypeSymbol
            override __.GetAddMethod(_nonPublicUnused) = inp.GetAddMethod(_nonPublicUnused) |> TxMethodDefinition
            override __.GetRemoveMethod(_nonPublicUnused) = inp.GetRemoveMethod(_nonPublicUnused) |> TxMethodDefinition

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

            override __.GetHashCode() = inp.GetHashCode()  |> NIX
            override __.Equals(that:obj) = inp.Equals(unwrapObj<EventInfo> that) 
            override __.ToString() = inp.ToString() |> NIX

            override __.GetRaiseMethod(nonPublic)                      = notRequired "GetRaiseMethod"
            override __.ReflectedType                                  = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                 = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)  = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)            = notRequired "IsDefined"

          interface IWraps<EventInfo> with 
              member x.Value = inp
        }

    and TxFieldDefinition(inp: FieldInfo) = 
        if inp = null then null else
        { new FieldInfo() with 

            override __.Name = inp.Name |> NIX
            override __.Attributes = inp.Attributes |> NIX
            override __.MemberType = inp.MemberType |> NIX
            override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition

            override __.FieldType = inp.FieldType |> TxTypeSymbol
            override __.GetRawConstantValue()  = inp.GetRawConstantValue() |> NIX

            override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

            override __.GetHashCode() = inp.GetHashCode()  |> NIX
            override __.Equals(that:obj) = inp.Equals(unwrapObj<FieldInfo> that) 
            override __.ToString() = inp.ToString() |> NIX
    
            override __.ReflectedType                                          = notRequired "ReflectedType"
            override __.GetCustomAttributes(inherited)                         = notRequired "GetCustomAttributes"
            override __.GetCustomAttributes(attributeType, inherited)          = notRequired "GetCustomAttributes"
            override __.IsDefined(attributeType, inherited)                    = notRequired "IsDefined"
            override __.SetValue(obj, _value, invokeAttr, binder, culture) = notRequired "SetValue"
            override __.GetValue(obj)                                         = notRequired "GetValue"
            override __.FieldHandle                                            = notRequired "FieldHandle"

          interface IWraps<FieldInfo> with 
              member x.Value = inp
        }

    and TxTypeSymbol(ty: Type) = 
        if ty = null then null else
      
        if ty.IsGenericType then 
            let args = Array.map TxTypeSymbol (ty.GetGenericArguments())
            ProvidedSymbolType(Generic (TxTypeDefinition (ty.GetGenericTypeDefinition())), Array.toList args)  :> Type
        elif ty.HasElementType then 
            let ety = TxTypeSymbol (ty.GetElementType()) 
            if ty.IsArray then 
                let rank = ty.GetArrayRank()
                if rank = 1 then ProvidedSymbolType(SDArray,[ety]) :> Type
                else ProvidedSymbolType(Array rank,[ety]) :> Type
            elif ty.IsPointer then ProvidedSymbolType(Pointer,[ety]) :> Type
            elif ty.IsByRef then ProvidedSymbolType(ByRef,[ety]) :> Type
            else ty
        elif ty.IsGenericParameter then ty
        else
            TxTypeDefinition ty

    and TxTypeDefinition(inp: Type) =
      if inp = null then null else
      txTable.Get inp <| fun () ->
        let isTarget =  (inp.Attributes &&& enum (int32 TypeProviderTypeAttributes.IsErased) <> enum 0)

        if not isTarget then 
            // Don't wrap types that aren't being translated. This applies particularly to types
            // which F# quotations and the F# compiler are sensitive too such as System.Tuple and System.Int32.
            inp  
        else
            { new Type() with 
                override __.Name = inp.Name |> NIX
                override __.Assembly = inp.Assembly |> TxAssembly isTarget
                override __.FullName = inp.FullName |> TxFullTypeName isTarget
                override __.Namespace = inp.Namespace |> TxNamespaceName isTarget
                override __.DeclaringType = inp.DeclaringType |> TxTypeDefinition
                override __.MemberType = inp.MemberType |> NIX

                override __.BaseType = inp.BaseType |> TxTypeSymbol
                override __.GetInterfaces() = inp.GetInterfaces() |> Array.map TxTypeSymbol

                override __.GetConstructors(bindingAttrUnused) = inp.GetConstructors (bindingAttrUnused) |> Array.map TxConstructorDefinition
                override __.GetMethods(bindingAttrUnused) = inp.GetMethods(bindingAttrUnused) |> Array.map TxMethodDefinition
                override __.GetField(name, bindingAttrUnused) = inp.GetField(name, bindingAttrUnused) |> TxFieldDefinition
                override __.GetFields(bindingAttrUnused) = inp.GetFields(bindingAttrUnused) |> Array.map TxFieldDefinition
                override __.GetEvent(name, bindingAttrUnused) = inp.GetEvent(name, bindingAttrUnused) |> TxEventDefinition
                override __.GetEvents(bindingAttrUnused) = inp.GetEvents(bindingAttrUnused) |> Array.map TxEventDefinition
                override __.GetProperties(bindingAttrUnused) = inp.GetProperties(bindingAttrUnused) |> Array.map TxPropertyDefinition
                override __.GetMembers(bindingAttrUnused) = inp.GetMembers(bindingAttrUnused) |> Array.map TxMemberDefinition
                override __.GetNestedTypes(bindingAttrUnused) = inp.GetNestedTypes(bindingAttrUnused) |> Array.map TxTypeSymbol
                override __.GetNestedType(name, bindingAttrUnused) = inp.GetNestedType(name, bindingAttrUnused) |> TxTypeSymbol

                override __.GetPropertyImpl(name, bindingAttrUnused, binderUnused, returnTypeUnused, typesUnused, modifiersUnused) = 
                    inp.GetProperty(name, bindingAttrUnused) |> TxPropertyDefinition
                    // inp.GetPropertyImpl(name, bindingAttrUnused, binderUnused, returnTypeUnused, typesUnused, modifiersUnused) |> TxPropertyDefinition
        
                // Every implementation of System.Type must meaningfully implement these
                override this.MakeGenericType(args) = ProvidedSymbolType(SymbolKind.Generic this, Array.toList args) :> Type
                override this.MakeArrayType() = ProvidedSymbolType(SymbolKind.SDArray, [this]) :> Type
                override this.MakeArrayType arg = ProvidedSymbolType(SymbolKind.Array arg, [this]) :> Type
                override this.MakePointerType() = ProvidedSymbolType(SymbolKind.Pointer, [this]) :> Type
                override this.MakeByRefType() = ProvidedSymbolType(SymbolKind.ByRef, [this]) :> Type

                override __.GetAttributeFlagsImpl() = inp.Attributes |> NIX

                override __.IsArrayImpl() = inp.IsArray |> NIX
                override __.IsByRefImpl() = inp.IsByRef |> NIX
                override __.IsPointerImpl() = inp.IsPointer |> NIX
                override __.IsPrimitiveImpl() = inp.IsPrimitive |> NIX
                override __.IsCOMObjectImpl() = inp.IsCOMObject |> NIX
                override __.IsGenericType = inp.IsGenericType |> NIX
                override __.IsGenericTypeDefinition = inp.IsGenericTypeDefinition |> NIX

                override __.HasElementTypeImpl() = inp.HasElementType |> NIX

                override __.UnderlyingSystemType = inp.UnderlyingSystemType |> TxTypeSymbol
                override __.GetGenericArguments() = inp.GetGenericArguments() |> Array.map TxTypeSymbol
                override __.GetGenericTypeDefinition() = inp.GetGenericTypeDefinition() |> TxTypeDefinition
                override __.GetCustomAttributesData() = inp.GetCustomAttributesData() |> TxCustomAttributesData

                override __.GetHashCode() = inp.GetHashCode()  |> NIX
                override __.Equals(that:obj) = inp.Equals(unwrapObj<Type> that) 
                override __.ToString() = inp.ToString() |> NIX

                override __.GetMember(name,mt,bindingAttrUnused)                                                      = notRequired "GUID"
                override __.GUID                                                                                      = notRequired "GUID"
                override __.GetMethodImpl(name, bindingAttrUnused, binder, callConvention, types, modifiers)          = notRequired "GetMethodImpl"
                override __.GetConstructorImpl(bindingAttrUnused, binder, callConvention, types, modifiers)           = notRequired "GetConstructorImpl"
                override __.GetCustomAttributes(inherited)                                                            = notRequired "GetCustomAttributes"
                override __.GetCustomAttributes(attributeType, inherited)                                             = notRequired "GetCustomAttributes"
                override __.IsDefined(attributeType, inherited)                                                       = notRequired "IsDefined"
                override __.GetInterface(name, ignoreCase)                                                            = notRequired "GetInterface"
                override __.Module                                                                                    = notRequired "Module" : Module 
                override __.GetElementType()                                                                          = notRequired "GetElementType"
                override __.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters) = notRequired "InvokeMember"
                override __.AssemblyQualifiedName                                                                     = notRequired "AssemblyQualifiedName"

              interface IWraps<Type> with 
                  member x.Value = inp
            }

    /// Transform a provided member definition
    and TxMemberDefinition(inp: MemberInfo) =
        if inp = null then null else
        match inp with 
        | :? MethodInfo as x -> (TxMethodDefinition x) :> MemberInfo
        | :? PropertyInfo as x -> (TxPropertyDefinition x) :> MemberInfo
        | :? EventInfo as x -> (TxEventDefinition x) :> MemberInfo
        | :? FieldInfo as x -> (TxFieldDefinition x) :> MemberInfo
        | :? Type as x -> (TxTypeDefinition x) :> MemberInfo
        | _ -> failwith "unknown member kind"


    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: IProvidedNamespace) = 
        { new IProvidedNamespace with
            override __.GetNestedNamespaces() = inp.GetNestedNamespaces() |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName |> TxNamespaceName true
            override __.GetTypes() = inp.GetTypes() |> Array.map TxTypeDefinition
            override __.ResolveTypeName(typeName) =  inp.ResolveTypeName(typeName) |> TxTypeDefinition
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ITypeProvider) = 
        { new ITypeProvider with 
            override __.GetNamespaces() = inp.GetNamespaces() |> Array.map TxNamespaceDefinition

            override __.GetInvokerExpression(syntheticMethodBase, parameters) = 
                let syntheticMethodBase2 = 
                    match syntheticMethodBase with 
                    | :? MethodInfo as x -> unwrap x :> MethodBase
                    | :? ConstructorInfo as x -> unwrap x :> MethodBase
                    | _ -> syntheticMethodBase
                let parameterVars2 = 
                    [| for p in parameters do 
                          match p with 
                          | Quotations.Patterns.Var(v) ->  yield Quotations.Var(v.Name, unwrap v.Type)
                          | _ -> failwith "unexpected non-var" |]
                let parameters2 = [| for v in parameterVars2 -> Quotations.Expr.Var v |] 
                let tab = Map.ofSeq (Array.zip parameterVars2 parameters)
                let q2 = inp.GetInvokerExpression(syntheticMethodBase2, parameters2) 
                let q = q2.Substitute (tab.TryFind)
                q

            override __.GetStaticParameters(typeWithoutArguments) = 
                inp.GetStaticParameters(unwrap typeWithoutArguments) |> Array.map TxStaticParameter

            override __.ApplyStaticArguments(typeWithoutArguments, typePathWithArguments, objs) = 
                let inpApplied = inp.ApplyStaticArguments(unwrap typeWithoutArguments, XIN typePathWithArguments, objs) 
                let dataContextMethod =
                    inpApplied.GetMethods()
                    |> Array.find (fun m -> m.IsStatic && m.Name = "GetSample")
                let expr = tp1.GetInvokerExpression(dataContextMethod, [| |])
                let dataContextObj = Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter.EvaluateQuotation(expr)
                let inpWrapped = inpApplied |> TxTypeSymbol
                let staticTypeOfDataContextObj = expr.Type
                contextObjs.[inpWrapped] <- (dataContextObj, expr.Type)
                inpWrapped

            override __.GetGeneratedAssemblyContents(assembly) = 
                inp.GetGeneratedAssemblyContents(assembly)

            [<CLIEvent>]
            override __.Invalidate = inp.Invalidate 

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
    
    TxTypeProviderDefinition(tp1)