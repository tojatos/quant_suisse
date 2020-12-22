namespace Lectures

module Lecture1 =
    let fst (x, _, _) = x
    let mid (_, x, _) = x
    let lst (_, _, x) = x
    
    let flipX (x, y) = (-x, y)
    let flipY (x, y) = (x, -y)

