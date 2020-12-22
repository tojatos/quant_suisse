module Lectures.Tests

open NUnit.Framework
open Lecture1

[<TestFixture>]
type Lecture1Tests () =
    
    [<Test>]
    member this.TripleTupleFunctions () =
        let tuple = (1, 2, 3)
        Assert.AreEqual(fst tuple, 1);
        Assert.AreEqual(mid tuple, 2);
        Assert.AreEqual(lst tuple, 3)
        
    member this.CoordinateTupleFunctions () =
        let v = (2, 4)
        Assert.AreEqual(flipX v, (-2, 4))
        Assert.AreEqual(flipY v, (2, -4))
//        Assert.AreEqual(rotate v 30, (2, -4))
