module WordCount.Tests

open NUnit.Framework
open FunctionLib
open System.IO
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type NUnitTest() =

  //100 random words from some online random word generator...
  let words = ["witty"; "tenuous"; "guarantee"; "breathe"; "utopian"; "meaty"; "grass"; "offer"; "astonishing"; "shape"; "lovely"; "ignore"; "private"; "curtain"; "ruin"; "unite"; "towering"; "knowing"; "malicious"; "hospital"; "curvy"; "defective"; "week"; "frighten"; "trick"; "lumber"; "omniscient"; "powerful"; "motion"; "warlike"; "abstracted"; "float"; "cherry"; "creature"; "famous"; "behave"; "governor"; "carve"; "hard"; "chess"; "hurry"; "son"; "juggle"; "quill"; "spot"; "flesh"; "trousers"; "holistic"; "freezing"; "abhorrent"; "wheel"; "devilish"; "delightful"; "influence"; "guess"; "confuse"; "coat"; "count"; "cynical"; "dazzling";     "hard-to-find"; "title"; "resolute"; "alike"; "rat"; "useless"; "zealous"; "lumpy"; "mean"; "wicked"; "yak"; "wealthy"; "eager"; "cool"; "arch"; "mountainous"; "destroy"; "mundane"; "verse"; "chief"; "table"; "past"; "stupendous"; "earthy"; "driving"; "fang";     "high-pitched"; "wretched"; "incredible"; "smart"; "possible"; "trail"; "fruit"; "wild"; "tangy"; "hurried"; "popcorn"; "evasive"; "sister"; "reproduce"; 
  ]
  
  let punctCs = Array.toList [|'!';'@';'#';'&';'(';')';'–';'[';'{';'}';']';':';';';'‘';',';'?';'/';'*'|]

  let space = Array.toList [|' ';'\t';'\r';'\n'|]
    
  let intGenerator = Arb.generate<int>
  
  [<Property>]
  let ``Removing punctuations from words without punctuations should give the same string`` () =
    let sample = Gen.elements words |> Gen.sample 100 10
    sample = (rmPunctuations sample)

  [<Property>]
  let ``Space, tab, newline and carriage return are all treated as whitespace`` () =
    let sampleWords = Gen.elements words |> Gen.sample 100 10
    let sampleSpaces = Gen.elements space |> Gen.sample 4 10
    
    let oracle = sampleWords
    let sample = (sampleWords,sampleSpaces) ||> List.map2 (fun s1 s2 -> [s1;s2.ToString()]) |> List.collect id |> List.fold (fun acc s -> acc+s) ""

    oracle = splitOnSpace [sample]

  [<Property>]
  let ``Should handle strings of random words with amount of spaces and varying leading and trailing punctutations``(k : int) =

    let sample = Gen.elements words |> Gen.sample 100 1000
    let oracle = sample |> Seq.groupBy id |> Seq.map(fun (k,v) -> (k,Seq.length v)) |> Seq.sortBy fst |> Seq.toList

    let spaces () = 
      Gen.sample k 1 intGenerator |> List.map (fun i -> String.init ((abs i)+1) (fun _ -> sprintf "%s" " ")) |> List.head
    
    let punctuations () = 
      let n = abs (List.head (Gen.sample k 1 intGenerator))
      let puncts = List.map (fun c -> c.ToString()) punctCs
      let m = List.length puncts
      let ps = Gen.elements puncts |> Gen.sample m n |> Seq.fold (fun acc s -> acc+s) "" 
      ps
  

    let sampleString = 
      sample |> 
      Seq.collect (fun w -> [punctuations();w;punctuations();(spaces())]) |> 
      Seq.fold (fun acc s -> acc+s) ""

    let actual = eval [sampleString] |> List.sortBy fst
    actual = oracle
