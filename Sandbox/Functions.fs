let apply f v =
    match v with 
    | Some value -> Some(f value)
    | None -> None
    

