#if INTERACTIVE
#r @"..\..\bin\FSharp.Data.dll"
#r @"..\..\bin\FSharp.Data.DesignTime.dll"
#r @"..\..\bin\TPCombinators.dll"
#else
module TPCombinators.ChainTests
#endif

open System
open System.IO
//open NUnit.Framework

module HyperlinkTests = 
    type Original = FSharp.Data.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let orows = Original.GetSample().Rows 
    let olinks = orows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Unsimplified1 = UnsimplifiedExample1.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let urows = Unsimplified1.GetSample().Rows 
    let ulinks = urows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Unsimplified2 = UnsimplifiedExample2.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let urows2 = Unsimplified2.GetSample().Rows 
    let ulinks2 = urows2 |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Simplified1 = SimplifiedExample1.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let rows = Simplified1.GetSample().Rows 
    let links = rows |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray

    type Simplified2 = SimplifiedExample2.CsvProvider<"""C:\Users\t-anstev\Documents\Visual Studio 2013\Projects\TPCombinators\tests\TPCombinators.Tests\hyperlinks.csv""">
    let rows2 = Simplified2.GetSample().Rows 
    let links2 = rows2 |> Seq.map (fun x -> x.Hyperlink) |> Seq.toArray


module UseOriginalWithStaticParameters = 
    type Dbp = FSharp.Data.DbPediaProvider<"en">

    let ctxt = Dbp.GetDataContext()

    let v1 : string = ctxt.Ontology.Holiday.Individuals.``Anzac Day``.``abstract``
    let v2 : string = ctxt.Ontology.Holiday.Individuals.``420 (cannabis culture)``.``abstract``

module UseChainedWithStaticParameters = 
    type Dbp = Chained.DbPediaProvider<"en">

    let ctxt = Dbp.GetDataContext()

    let v1 : int = ctxt.Ontology.Holiday.Individuals.``Anzac Day``.abstract_look_ma_i_know_it_has_length_537
    let v2 : int = ctxt.Ontology.Holiday.Individuals.``420 (cannabis culture)``.abstract_look_ma_i_know_it_has_length_277

module UseOriginalWithoutStaticParameters = 

    let ctxt = FSharp.Data.DbPedia.GetDataContext()
    let v1 : string = ctxt.Ontology.Holiday.Individuals.``Anzac Day``.``abstract``


module UseChainedWithoutStaticParameters = 

    let ctxt = Chained.DbPedia.GetDataContext()
    
    let v1 : int = ctxt.Ontology.Holiday.Individuals.``Anzac Day``.``abstract`` // TODO: the translation should apply here too

