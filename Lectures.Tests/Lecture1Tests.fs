module Lectures.Tests

open System
open NUnit.Framework
open Lecture1

[<TestFixture>]
type Lecture1Tests () =
    
    [<Test>]
    member this.TripleTupleFunctions () =
        let tuple = (1, 2, 3)
        Assert.AreEqual(1, fst tuple)
        Assert.AreEqual(2, mid tuple)
        Assert.AreEqual(3, lst tuple)
        
    [<Test>]
    member this.CoordinateTupleFlips () =
        let v = (2., 4.)
        Assert.AreEqual((-2., 4.), flipX v)
        Assert.AreEqual((2., -4.), flipY v)
        
    [<Test>]
    member this.CoordinateTupleRotates () =
        let round5 (x: float) = Math.Round(x, 5)
        let round5t (x, y) = (round5 x, round5 y)
        let v = (0., 1.)
        Assert.AreEqual(v, round5t (rotate v (2. * Math.PI)))
        Assert.AreEqual((0, -1), round5t (rotate v (Math.PI)))
        Assert.AreEqual((-1, 0), round5t (rotate v (Math.PI / 2.)))
        Assert.AreEqual((1, 0), round5t (rotate v (-Math.PI / 2.)))
        
    [<Test>]
    member this.RemoveDupsWorks () =
        let xs1 = [1; 2; 3; 4]
        let xs2 = [1; 2; 2; 4]
        let xs3 = [-20; -3; 5; 32; 32; -20; 5]
        CollectionAssert.AreEqual(xs1, removeDups xs1)
        CollectionAssert.AreEqual([1; 2; 4], removeDups xs2)
        CollectionAssert.AreEqual([-20; -3; 5; 32], removeDups xs3)
        
    [<Test>]
    member this.RemoveDups2Works () =
        let xs1 = [1; 2; 3; 4]
        let xs2 = [1; 2; 2; 4]
        let xs3 = [-20; -3; 5; 32; 32; -20; 5]
        CollectionAssert.AreEqual(xs1, removeDups2 xs1)
        CollectionAssert.AreEqual([1; 2; 4], removeDups2 xs2)
        CollectionAssert.AreEqual([-20; -3; 5; 32], removeDups2 xs3)
        