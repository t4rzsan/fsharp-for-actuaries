type Email = Email of string
type Name = Name of string
type StreetAddress = StreetAddress of string
type PostalCode = PostalCode of string
type City = City of string
type Address = 
    {
        StreetAddress: StreetAddress;
        PostalCode: PostalCode;
        City: City;
    }
type Customer =
    {
        Email: Email;
        Name: Name;
        Address: Address;
    }

type ValidationError =
    | EmailValidation of string
    | NamesValidation
    | AddressValidation of string

let validateEmailWithResult doFail (customer: Customer) = 
    if doFail then
        Error(EmailValidation("The email is not in the correct format."))
    else
        Ok(customer)

let validateNamesWithResult doFail (customer: Customer) = 
    if doFail then
        Error(NamesValidation)
    else
        Ok(customer)

let validateAddressWithResult doFail (customer: Customer) = 
    if doFail then
        Error(AddressValidation("Street address is invalid."))
    else
        Ok(customer)
        
let (>=>) a b x =
    match a x with
    | Ok v -> b v
    | Error reason -> Error(reason)

let (>>=) x f = Result.bind f x

let validateWithOk =
    validateEmailWithResult false
    >=> validateNamesWithResult false
    >=> validateAddressWithResult false
    
let validateWithError = 
    validateEmailWithResult true // Force an error
    >=> validateNamesWithResult false
    >=> validateAddressWithResult false

let customer =
    {
        Email = (Email "jakob@domain.com");
        Name = (Name "Jakob Christensen");
        Address = 
            {
                StreetAddress = (StreetAddress "1 Composition St");
                PostalCode = (PostalCode "123456");
                City = (City "Functional City")
            };
    }

printfn "This one is ok: %A" (validateWithOk customer)
printfn "Ths on failed: %A" (validateWithError customer)

let validateWithError2 x =
    x
    |> (validateEmailWithResult true)
    >>= (validateNamesWithResult false)
    >>= (validateAddressWithResult false)

let t1 x = x |> (validateEmailWithResult true)
let t2 x = (t1 x) >>= (validateNamesWithResult false)
let t3 x = (t2 x) >>= (validateAddressWithResult false)
