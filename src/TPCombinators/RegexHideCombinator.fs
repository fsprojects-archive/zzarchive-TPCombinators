module FSharp.ProvidedTypes.RegexHideCombinator

open System
open System.Text
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System.Collections.Generic
open Microsoft.FSharp.Core.CompilerServices
open FSharp.ProvidedTypes.GeneralCombinators
open FSharp.ProvidedTypes.SimplifiedAlgebra
open System.Text.RegularExpressions


let MatchOptionRegex (inp: string, pattern: string option) =
    match pattern with
      | Some(str) -> Regex.IsMatch(inp, str)
      | None -> true

let private FilterProvidedProperties rawHiding (tp: ISimpleTypeProvider) = 

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    let TxAssembly (a:Assembly) = thisAssembly 

    let TxCustomAttributes (inp: seq<CustomAttributeData>) = inp

    let rec TxStaticParameter(inp: ISimpleStaticParameter) = 
        { new ISimpleStaticParameter with 
            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.OptionalValue = inp.OptionalValue
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }


    and TxParameter(inp : ISimpleParameter) = 
        { new ISimpleParameter with 
            override __.Name = inp.Name 
            override __.ParameterType = inp.ParameterType |> TxType
            override __.OptionalValue = inp.OptionalValue 
            override __.IsOut = inp.IsOut 
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }
 
    and TxConstructor(inp: ISimpleConstructor) = 
        { new ISimpleConstructor with
            override __.Parameters = inp.Parameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and FindHidingSettings (ty: ISimpleTypeDefinition) = 
      match ty, ty.DeclaringType with
      | (:? IWraps<string * bool * string option> as hiding), _ -> hiding.Value
      | _, Some ty -> FindHidingSettings ty
      | _ -> rawHiding

    and TxMethodFilter (declTy: ISimpleTypeDefinition) (inp: ISimpleMethod) =
        let (pattern, show, restriction)  = FindHidingSettings declTy

        //Never hide static methods
        if inp.IsStatic then 
            true
        //Don't hide if a restriction is passed and the type doesn't match it
        elif not (MatchOptionRegex(declTy.Name, restriction)) then
            true
        else
            //Either show or hide methods that match the regex depending on the value of "show"
            if inp.Name.[0..3] = "get_" && Regex.IsMatch(inp.Name.[4..], pattern) then
                show
            else 
                not show

    and TxPropertyFilter (declTy: ISimpleTypeDefinition) (inp: ISimpleProperty) = 
        let (pattern, show, restriction)  = FindHidingSettings declTy

        //If the property does not match the restriction regex, show it        
        if not (MatchOptionRegex(declTy.Name, restriction)) then
            true
        else
            //Else, show or hide matching properties depending on the value of "show"
            if Regex.IsMatch(inp.Name, pattern) then
                show
            else
                not show

    and TxMethod(inp: ISimpleMethod) =
        { new ISimpleMethod with 

            override __.Name              = inp.Name  
            override __.IsStatic          = inp.IsStatic  
            override __.Parameters        = inp.Parameters |> Array.map TxParameter
            override __.ReturnType        = inp.ReturnType  |> TxType
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }

    and TxAssociatedMethod(inp: ISimpleAssociatedMethod) =
        { new ISimpleAssociatedMethod with 
            override __.GetImplementation(parameters) = inp.GetImplementation(parameters)
        }


    and TxProperty(inp: ISimpleProperty) = 
        { new ISimpleProperty with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic 
            override __.PropertyType = inp.PropertyType |> TxType
            override __.GetMethod = inp.GetMethod |> Option.map TxAssociatedMethod
            override __.SetMethod = inp.SetMethod |> Option.map TxAssociatedMethod
            override __.IndexParameters = inp.IndexParameters |> Array.map TxParameter
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes

        }

    and TxEventDefinition(inp: ISimpleEvent) = 
        { new ISimpleEvent with 

            override __.Name = inp.Name 
            override __.IsStatic = inp.IsStatic 
            override __.EventHandlerType = inp.EventHandlerType |> TxType
            override __.AddMethod = inp.AddMethod |> TxAssociatedMethod
            override __.RemoveMethod = inp.RemoveMethod |> TxAssociatedMethod
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
        }

    and TxField(inp: ISimpleLiteralField) = 
        { new ISimpleLiteralField with 

            override __.Name = inp.Name 
            override __.FieldType = inp.FieldType |> TxType
            override __.LiteralValue  = inp.LiteralValue 
            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
        }

    and TxMember this (inp: ISimpleMember) =
        match inp with
        | Constructor ctor -> Some(Constructor(TxConstructor ctor))
        | Method meth -> Some(Method(TxMethod meth))
        | Field fld -> Some(Field(TxField fld))
        | Property prop when TxPropertyFilter this prop -> Some(Property(TxProperty prop))
        | Property prop -> None
        | Event evt -> Some(Event(TxEventDefinition evt))

    and TxType(inp: ISimpleType) = 
        match inp with 
        | TyApp(td, args) -> TyApp(TxTypeDefinitionReference td, Array.map TxType args)
        | TyArray(n, arg) -> TyArray(n, TxType arg)
        | TyPointer(arg) -> TyPointer(TxType arg)
        | TyByRef(arg) -> TyByRef(TxType arg)

    and TxTypeDefinitionReference inp = 
        match inp with
        | ISimpleTypeDefinitionReference.OtherTyDef _ -> inp
        | ISimpleTypeDefinitionReference.SimpleTyDef x -> SimpleTyDef (TxTypeDefinition x)

    and TxTypeDefinition (inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->
        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace 
            override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override this.Members = inp.Members |> Array.choose (TxMember this)
            override __.NestedTypes = inp.NestedTypes |> Array.map TxTypeDefinition
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map TxTypeDefinition

            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.ApplyStaticArguments(typePathWithArguments, objs) = 
                //If an empty pattern is passed then we expect to retrieve it from the static arguments
                match rawHiding with
                | "", _,_ when inp.DeclaringType = None ->
                      inp |> ApplyStaticArgs(typePathWithArguments, objs)
                | _ -> 
                      inp.ApplyStaticArguments(typePathWithArguments, objs) |> TxTypeDefinition 
            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
        }

    and TxHidingTypeDefinition hiding (inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->
        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace 
            override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override this.Members = inp.Members |> Array.choose (TxMember this)
            override __.NestedTypes = inp.NestedTypes |> Array.map TxTypeDefinition
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map TxTypeDefinition

            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.ApplyStaticArguments(typePathWithArguments, objs) = 
                failwith "Nested types of types with static arguments cannot have static arguments"
            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
          interface IWraps<string * bool * string option> with
            override __.Value = hiding 
        }

    and findStaticArgs(staticParams: ISimpleStaticParameter[], name: string, paramType: Type) =
        staticParams
        |> Array.findIndex( fun (x: ISimpleStaticParameter) -> 
                                 x.Name = name && x.ParameterType = paramType)

    and ApplyStaticArgs (typePathWithArguments, objs) (inp: ISimpleTypeDefinition) =
        
            let regexIndex = findStaticArgs(inp.StaticParameters, "Regex", typeof<string>)
            let showIndex = findStaticArgs(inp.StaticParameters, "Show", typeof<bool>)

            //Try to find the optional "Restriction" static argument
            let restrictionIndex = Array.tryFindIndex (fun (x: ISimpleStaticParameter) -> 
                                                         x.Name="Restriction" && x.ParameterType=typeof<string>) 
                                                      inp.StaticParameters
            
            let pattern = objs.[regexIndex] :?> string
            let show = objs.[showIndex] :?> bool
            let restriction =
                match restrictionIndex with
                | None -> None
                | Some(index) -> Some (objs.[index] :?> string)

            let newHiding = (pattern, show, restriction)
            inp.ApplyStaticArguments(typePathWithArguments, objs)
            |> TxHidingTypeDefinition newHiding
      
    /// Transform a provided namespace definition
    let rec TxNamespaceDefinition (inp: ISimpleNamespace) = 
        { new ISimpleNamespace with
            override __.NestedNamespaces = inp.NestedNamespaces |> Array.map TxNamespaceDefinition
            override __.NamespaceName = inp.NamespaceName 
            override __.TypeDefinitions = inp.TypeDefinitions |> Array.map TxTypeDefinition
            override __.GetTypeDefinition(name) =  inp.GetTypeDefinition(name) |> Option.map TxTypeDefinition
         }

    /// Transform an input ITypeProvider
    let TxTypeProviderDefinition (inp: ISimpleTypeProvider) = 
        { new ISimpleTypeProvider with 
            override __.Namespaces = inp.Namespaces |> Array.map TxNamespaceDefinition

            [<CLIEvent>]
            override __.Invalidate = inp.Invalidate 

          interface System.IDisposable with 
            override x.Dispose() = inp.Dispose()
        }
    
    TxTypeProviderDefinition tp

let HideProperties (pattern: string) (tp: ISimpleTypeProvider) =
    FilterProvidedProperties(pattern, false, None) (tp: ISimpleTypeProvider)

let HidePropertiesInType (pattern: string, declaring: string) (tp: ISimpleTypeProvider) =
    FilterProvidedProperties(pattern, false, Some declaring) (tp: ISimpleTypeProvider)

let ShowProperties (pattern: string) (tp: ISimpleTypeProvider) =
    FilterProvidedProperties(pattern, true, None) (tp: ISimpleTypeProvider)

let ShowPropertiesInType (pattern: string, declaring: string) (tp: ISimpleTypeProvider) =
    FilterProvidedProperties(pattern, true, Some declaring) (tp: ISimpleTypeProvider)

//Used when adding static parameters to the TP
let HideStatic (tp: ISimpleTypeProvider)=
    FilterProvidedProperties("", false, None) tp

