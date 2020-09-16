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

type Blabla =
    | EmailValidation
    | NamesValidation
    | AddressValidation

let validateEmailWithResult doFail (customer: Customer) = 
    if doFail then
        Error(Blabla.EmailValidation)
    else
        Ok(customer)

let getCustomer () : Result<Customer, Blabla> =
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
    |> Ok

let customer = getCustomer ()
let res = Result.bind (validateEmailWithResult false) customer
printfn "%A" res

let (>=>) a b x =
    match a x with
    | Ok v -> b v
    | Error reason -> Error(reason)

    