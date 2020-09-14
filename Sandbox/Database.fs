open System.Data.Common
open System.Data.SqlClient

type PolicyNumber = PolicyNumber of string

type Policy =
    {
        PolicyNumber: PolicyNumber;
        Premium: decimal option;
    }

let getValue fieldName (reader: DbDataReader) =
    let ordinal = reader.GetOrdinal(fieldName)
    if reader.IsDBNull(ordinal) then
        None
    else
        Some(reader.GetFieldValue(ordinal))

let getPolicies () =
    let getPremium = getValue "Premium"

    let policies = 
        seq {
            use cn = new SqlConnection(@"Integrated Security=SSPI;Persist Security Info=False;Initial Catalog=FSharpForActuaries;Data Source=.\SQLEXPRESS")
            use cmd = new SqlCommand("SELECT PolicyNumber, Premium FROM Policy", cn)
            cn.Open()
            use reader = cmd.ExecuteReader()
            while reader.Read() do
                yield {
                    PolicyNumber = (PolicyNumber (string reader.["PolicyNumber"]));
                    Premium = reader |> getPremium
                }
        }
    
    policies

let policies = getPolicies ()

printfn "%A" (policies |> Seq.skip 1 |> Seq.head)

