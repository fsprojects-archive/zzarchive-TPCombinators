// A simplified view of the algebra of provided constructs.
//
// TODO: Implement ISimpleTypeProvider --> ITypeProvider (injecting implementations of the 'crud')
// TODO: Implement ITypeProvider --> ISimpleTypeProvider (eliding the 'crud')

module FSharp.ProvidedTypes.SimplifiedAlgebra

open System
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

type ISimpleStaticParameter  = 
    abstract Name : string
    abstract ParameterType : ISimpleType 
    abstract OptionalValue : obj option
    abstract CustomAttributes : CustomAttributeData[]

and ISimpleParameter  = 
    abstract Name : string
    abstract ParameterType : ISimpleType 
    abstract OptionalValue : obj option
    abstract IsIn : bool
    abstract IsOut : bool
    abstract CustomAttributes : CustomAttributeData[]
 
and ISimpleConstructor = 
    abstract DeclaringType    : ISimpleTypeDefinition
    abstract Parameters       : ISimpleParameter[]
    abstract CustomAttributes : CustomAttributeData[]

and ISimpleMethod =
    abstract Name              : string
    abstract Parameters        : ISimpleParameter[]
    abstract DeclaringType     : ISimpleTypeDefinition 
    abstract ReturnType        : ISimpleType 
    abstract CustomAttributes  : CustomAttributeData[]

and ISimpleProperty = 
    abstract Name            : string
    abstract DeclaringType   : ISimpleTypeDefinition
    abstract PropertyType    : ISimpleType
    abstract GetMethod       : ISimpleMethod option
    abstract SetMethod       : ISimpleMethod option
    abstract IndexParameters : ISimpleParameter[]
    abstract CustomAttributes : CustomAttributeData[]

and ISimpleEvent = 
    abstract Name : string
    abstract DeclaringType : ISimpleTypeDefinition
    abstract EventHandlerType : ISimpleType
    abstract AddMethod : ISimpleMethod
    abstract RemoveMethod : ISimpleMethod
    abstract CustomAttributes : CustomAttributeData[]

and ISimpleField  = 
    abstract Name : string  
    abstract DeclaringType : ISimpleTypeDefinition 
    abstract FieldType : ISimpleType
    abstract LiteralValue : obj option
    abstract CustomAttributes : CustomAttributeData[]

and ISimpleType = 
    | TyApp of ISimpleTypeDefinition * ISimpleType[]
    | TyArray of int * ISimpleType
    | TyPointer of ISimpleType

and ISimpleTypeDefinition =
    abstract Name : string
    abstract Assembly : System.Reflection.Assembly
    abstract Namespace : string
    abstract DeclaringType : ISimpleTypeDefinition option

    abstract BaseType : ISimpleType option
    abstract Interfaces : ISimpleType[]

    abstract Constructors : ISimpleConstructor[]
    abstract Methods : ISimpleMethod[]
    abstract Fields : ISimpleField[]
    abstract Events : ISimpleEvent[]
    abstract Properties : ISimpleProperty[]
    abstract NestedTypes : ISimpleTypeDefinition[]

    abstract GetField : name:string -> ISimpleField
    abstract GetEvent: name:string -> ISimpleEvent
    abstract GetProperty : name:string -> ISimpleProperty
    abstract GetNestedType : name:string -> ISimpleTypeDefinition
    
    abstract IsErased : bool

    abstract StaticParameters : ISimpleStaticParameter[]
    abstract ApplyStaticArguments : string[] * obj[] -> ISimpleTypeDefinition  
    abstract CustomAttributes : CustomAttributeData[]
    //abstract UnderlyingSystemType = inp.UnderlyingSystemType |> TxTypeSymbol


and ISimpleNamespace = 
    abstract NestedNamespaces : ISimpleNamespace[]
    abstract NamespaceName : string
    abstract TypeDefinitions : ISimpleTypeDefinition[]
    abstract GetType : name:string -> ISimpleTypeDefinition

and ISimpleTypeProvider = 
    abstract Namespaces : ISimpleNamespace[]
    abstract GetInvokerExpression : ISimpleMethod * Expr[] -> Expr
    [<CLIEvent>]
    abstract Invalidate : IEvent<unit>
    inherit System.IDisposable 


