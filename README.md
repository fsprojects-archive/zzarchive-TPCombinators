[![Issue Stats](http://issuestats.com/github/fsprojects/TPCombinators/badge/issue)](http://issuestats.com/github/fsprojects/TPCombinators)
[![Issue Stats](http://issuestats.com/github/fsprojects/TPCombinators/badge/pr)](http://issuestats.com/github/fsprojects/TPCombinators)

# Type Provider Combinator Experiments

Type Provider Combinators  have potential applications both to entity graphs (chaining information/schema transformations and extractions), Hive (caching, polling etc., others too) and many other possibilities.  

TPCs are also conceptually interesting because we can use them to transform/filter/adjust our static views of very large information spaces such as Freebase and DbPedia – or user-specific information spaces such as big-data repositories. While TPCs are just one example of the idea, techniques to “transform and simplify our view of the world around us” are going to be hugely important in the information-rich world.  Because of this we should aim to be writing up the ideas we generate both as a tech report and (eventually) as a paper to submit.

We’ve talked about TPCs for a long time, but never actually implemented one!  So I did a sample type provider combinator implementation to prove to myself that this TPCs actually even possible.  The sample is here.



# Rough Design Note on Type Provider Combinators

Implementing type providers correctly is hard. Often, a type provider implementation ends up being “a pile of features”, many of which are similar across different type providers.  For example, 

-	many type providers implement some kind of caching of metadata

-	many type providers could theoretically benefit from “background polling” for changes in metadata

Further, some type providers feel like they implement functionality which should “compose” nicely:

-	For example, a CSV file which is named as a string value in a FileSystem type provider feels as if it should be usable as static schema using the CSV type provider.

Finally, it feels interesting to think of operations which manipulate entire entity graphs, libraries or other information spaces, e.g.

-	Operations which filter out parts of a library which we think are “dangerous” and don’t want to see

-	Operations which add asynchronous or streaming versions of methods/properties to libraries

-	Operations which add new functionality to every type in an entity graph (whether that is DbPedia or Freebase, and independent of the details of how the entity graph and its schema is projected into F#)

A potential “power user” approach to problems like this is to allow type provider to be specified compositionally, using “type provider combinators”.  

For example, we would like to specify 

                AddUnits<”Units”,Hide<”Society”,AddAsync< FSharp.Data.FreebaseDataProvider>>>

to 

-	add units of measure to a basic implementation of Freebase (using some default schema)

-	hide the “Society” information, and 

-	add async property support.  

Here operations like AddUnits, Hide and AddAsync are take a type provider and produce a type provider – they are combinators on type provider.

Remember that the things we are transforming here are “big” 
-	A type provider represents a capability to access both the metadata and the data of a protocol or data source where the metadata is accessed in a scalable, on-demand way.  

-	So a type provider combinator represents a transformation of a capability to access an information space in a scalable, on-demand way.


From 1000 feet up, a type provider provides namespaces containing provided type definitions. Provided type definitions describe .NET 2.x type definitions. In addition, 

-	Static parameters may be specified on type definitions 

-	Generic type definitions are not allowed

-	An “erasure function” must be given for provided types

### What makes a correct Type Provider Combinator

Some initial criteria

-	Useful

-	Correctness

-	Consistency

-	Preserves laziness (not more eager than necessary)

-	Scales

A type provider combinator can be hard to implement correctly because it must provide a consistent transformation of an information graph.

### The ITypeProvider algebra

The representation of type providers loaded by the F# compiler is “ITypeProvider”.  We call this object model the “ITypeProvider algebra”.  BrieflyL

-	DLLs contain one or more types which implement ITypeProvider and are marked with a TypeProvider attribute

-	An ITypeProvider instance provides a number of type definitions organized into namespaces.

-	Type, method, field, property and constructor definitions are represented using System.Type , System.Reflection.MethodInfo and friends from .NET reflection

-	Types (e.g. array types as opposed to type definitions) are also represented using System.Type 

-	Static parameters are specified and expanded using additional static methods on ITypeProvider.


### A Simple Type Provider Combinator over the ITypeProvider algebra

The project https://github.com/fsprojects/TPCombinators shows a simple “clone and rename-the-namespace” combinatory over the ITypeProvider algebra. See [here](https://github.com/fsprojects/TPCombinators/blob/master/src/TPCombinators/CloneCombinator.fs)


### Example of using a Type Provider Combinator

In this example, we copy+rename the namespace of the provided types from FSharp.Data CSVProvider:

     Clone CsvProvider, renaming FSharp.Data --> MySpace

Here’s the specification of the composition of the type provider:

    let Example1 config = 
    
         // This fetches an instance of the type provider we wish to transform. 
        let CsvProvider = 
            let FSharpDataAssembly = typeof<FSharp.Data.CsvFile>.Assembly
            new ProviderImplementation.CsvProvider(ConfigForOtherTypeProvider(config, FSharpDataAssembly.Location))

        Clone("FSharp.Data", "MySpace", CsvProvider)

Here’s how the example is turned into a new “computed” type provider, in a new DLL

    [<TypeProvider>]
    type Example1Provider(config) = inherit TypeProviderExpression(Example1(config))

Note that this doesn’t expand the F# language, it’s just implementing type provider DLLs using combinators.

### Advanced: What’s wrong with the ITypeProvider algebra?

As seen from the sample, the ITypeProvider algebra has a number of problems.  

-	The System.Reflection objects have a large amount of “crud” methods which are never called by the F# compiler

-	There are numerous “unused” arguments, e.g. for binding flags. The F# compiler will never pass interesting values for these arguments

-	There is significant information duplication. For example “FullName” on System.Type can be deduced from “Name” and “Namespace”

-	Many of the implemented properties have uninteresting values, e.g. “MemberType” on members is entirely uninteresting.

-	“MemberInfo” is used as a base class, and must be preserved by transformations

-	The algebra conflates “types” and “type definitions”. The algebra of types (array types, generic instantiations) is hidden in a messy set of properties on System.Type

### Advanced: An alternative: the ISimpleTypeProvider algebra

The sample also includes a sample “simplified” algebra that represents a reduced information model that only contains the parts of the ITypeProvider algebra that we care about.  This makes it considerably easier to implement combinators. See [here](https://github.com/fsprojects/TPCombinators/blob/master/src/TPCombinators/SimplifiedAlgebra.fs)

A draft of an example combinatory is [here](https://github.com/fsprojects/TPCombinators/blob/master/src/TPCombinators/CloneCombinatorOverSimplifiedAlgebra.fs)

However, this is not yet complete as in order to use it we need operations to project in/out of the simplified world.
                ISimpleTypeProvider --> ITypeProvider
                ITypeProvider --> ISimpleTypeProvider


With these operations we could chain combinators over ISimpleTypeProvider like this:

                ITypeProvider --> ISimpleTypeProvider--> ISimpleTypeProvider--> ISimpleTypeProvider --> ITypeProvider


There are some problems with this though, it still needs work


### Maintainer(s)

- [@victor-dumitrescu](https://github.com/victor-dumitrescu)
- [@aastevenson](https://github.com/aastevenson)
- [@dsyme](https://github.com/dsyme)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)

