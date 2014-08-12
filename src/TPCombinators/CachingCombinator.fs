module FSharp.ProvidedTypes.CachingCombinator

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
open System.Security.Cryptography
open Nessos.FsPickler

type System.String with 
    member s.ReplacePrefix (s1:string, s2:string) =  
        if s.StartsWith(s1) then s2 + s.[s1.Length..] else s

type CachedField =
    {
        Name: string
//need to sort these out
//        FieldType: ISimpleType
        LiteralValue: obj
    }

type CachedTypeDefiniton = 
    {
        Name: string
        Fields: list<CachedField>
        NestedTypes: list<CachedTypeDefiniton>
    }

let CacheField (inp: ISimpleLiteralField) =
    {
        Name = inp.Name;
//        FieldType = inp.FieldType;
        LiteralValue = inp.LiteralValue
    }

let rec private CacheTypeDefinition (inp: ISimpleTypeDefinition) =
    {
        Name = inp.Name;
        Fields = inp.Fields |> List.ofSeq |>List.map CacheField
        NestedTypes = inp.NestedTypes |> List.ofSeq |> List.map CacheTypeDefinition
    }

let private hashString (plainText:string) = 
    let plainTextBytes = Encoding.UTF8.GetBytes(plainText)
    let hash = new SHA1Managed()
    let hashBytes = hash.ComputeHash(plainTextBytes)
    Convert.ToBase64String(hashBytes)
 
//hacky way of dealing with invalid file name chars
let private replaceChars(s: string) =
    s.Replace('/', 'ă').Replace('|', 'â')

let Cache (tp: ISimpleTypeProvider) = 

    let thisAssembly = typedefof<Utils.IWraps<_>>.Assembly

    // A table tracking how wrapped type definition objects are translated to cloned objects.
    let txTable = TxTable<ISimpleTypeDefinition, ISimpleTypeDefinition>()

    // The transformation we perform on the assembly. isTarget indicates if this is a provided object we should transform. 
    let TxAssembly (a:Assembly) = thisAssembly 

    let TxCustomAttributes (inp: seq<CustomAttributeData>) = inp

    let rec TxStaticParameter(inp: ISimpleStaticParameter) = 

        { new ISimpleStaticParameter with 

            override __.Name = inp.Name
            override __.ParameterType = inp.ParameterType
            override __.OptionalValue = inp.OptionalValue
            override __.CustomAttributes = inp.CustomAttributes  |> TxCustomAttributes
        }

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

    and TxTypeDefinition(inp: ISimpleTypeDefinition) =
      txTable.Get inp <| fun () ->

        { new ISimpleTypeDefinition with 
            override __.Name = inp.Name 
            override __.Assembly = inp.Assembly |> TxAssembly 
            override __.Namespace = inp.Namespace
            override __.DeclaringType = inp.DeclaringType |> Option.map TxTypeDefinition

            override __.BaseType = inp.BaseType |> Option.map TxType
            override __.Interfaces = inp.Interfaces |> Array.map TxType

            override __.Members = inp.Members
            override __.NestedTypes = inp.NestedTypes |> Array.map TxTypeDefinition
            override __.GetNestedType(name, declaredOnly) = inp.GetNestedType(name, declaredOnly) |> Option.map TxTypeDefinition

            override __.CustomAttributes = inp.CustomAttributes |> TxCustomAttributes
            override __.ApplyStaticArguments(typePathWithArguments, objs) = inp |> ApplyStaticArgs (typePathWithArguments, objs) 

            override __.StaticParameters = inp.StaticParameters |> Array.map TxStaticParameter
        }

    and ReconstructMember(inp: CachedField) = 
        ISimpleMember.Field { new ISimpleLiteralField with 

            override __.Name = inp.Name 
            override __.FieldType = ISimpleType.FromPrimitive(typeof<System.String>)
            override __.LiteralValue  = inp.LiteralValue 
            override __.CustomAttributes = Seq.empty
        } 
        
    and ReconstructTypeDefinition(inp: ISimpleTypeDefinition, decl: ISimpleTypeDefinition option) (cache: CachedTypeDefiniton) =
        let hasSameName (name: string) (nestedType: CachedTypeDefiniton) =
            nestedType.Name = name
        { new ISimpleTypeDefinition with 
            override this.Name = cache.Name
            override this.Assembly = inp.Assembly |> TxAssembly 
            override this.Namespace = inp.Namespace
            override this.DeclaringType = 
                match decl with
                    | None -> inp.DeclaringType
                    | x -> x

            override this.BaseType = inp.BaseType |> Option.map TxType
            override this.Interfaces = Array.empty

            override this.Members = cache.Fields |> Array.ofList |> Array.map ReconstructMember 
            override this.NestedTypes = cache.NestedTypes |> List.toArray |> Array.map (ReconstructTypeDefinition (inp, Some this))
            override this.GetNestedType(name, declaredOnly) = 
                    (fun name _ ->
                        (match List.tryFind (hasSameName name) cache.NestedTypes with
                            | Some nestedType -> nestedType |> (ReconstructTypeDefinition (inp, Some this)) |> Some
                            | None -> None
                                                                   
                        )) name declaredOnly

            override this.CustomAttributes = Seq.empty
            override this.ApplyStaticArguments(typePathWithArguments, objs) = failwith "won't fail, doesn't have any"

            override this.StaticParameters = Array.empty
        }

    and ApplyStaticArgs (typePathWithArguments, objs) (inp: ISimpleTypeDefinition) =

        let fileName = String.concat ";" typePathWithArguments |> hashString
        let path = String.Concat [Path.GetTempPath(); (replaceChars fileName); ".xml"]
        let binary = FsPickler.CreateXml()

        let applied = 
            try
                let pickle = System.IO.File.ReadAllBytes(path)
                let cache = binary.UnPickle<CachedTypeDefiniton> pickle
                cache |> (ReconstructTypeDefinition (inp, None))
            with
                | :?System.IO.FileNotFoundException ->
                    let appliedStaticArgs = inp.ApplyStaticArguments(typePathWithArguments, objs)
                    let cache = appliedStaticArgs |> (CacheTypeDefinition)
                    System.IO.File.WriteAllBytes(path, binary.Pickle cache)
                    appliedStaticArgs
        applied

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
  
   
//    let CacheNamespaceDefinition (cache: CachedTypeDefiniton) (inp: ISimpleNamespace) =
//        {
//            Name = inp.NamespaceName;
//            DeclaringType = Some cache;
//            Fields = Seq.empty
//            NestedTypes = inp.TypeDefinitions |> Seq.map (CacheTypeDefinition cache)
//        }
//
//    let CacheTypeProviderDefinition (cache: CachedTypeDefiniton) (inp: ISimpleTypeProvider) =
//        {
//            Name = inp.ToString();
//            DeclaringType = cache.DeclaringType;
//            Fields = Seq.empty;
//            NestedTypes = inp.Namespaces |> Seq.map (CacheNamespaceDefinition cache)
//        }
    
    
    let a = TxTypeProviderDefinition(tp)

//    let emptyCache = 
//        {Name="Sysobj"; DeclaringType=None; Fields=Seq.empty; NestedTypes=Seq.empty}
//    let cache = a |> (CacheTypeProviderDefinition emptyCache)

//    printfn "%A" cache
    
    a