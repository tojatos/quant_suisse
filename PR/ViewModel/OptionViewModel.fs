namespace ViewModel

type OptionKind =
    | Call
    | Put
    override x.ToString() = match x with | Call -> "Call" | _ -> "Put"

type OptionRecord =
    {
        Kind: OptionKind
        StockPrice: float
        Strike: float
        Expiry: System.DateTime
        r: float
        v: float
    }
 
//Representation of a Option to the UI
type OptionViewModel(input : OptionRecord) = 
    inherit ViewModelBase()

    let mutable userInput = input
    //let mutable value : Money option = None

    member this.Kind 
        with get() = userInput.Kind
        and set(x) = 
            userInput <- {userInput with Kind = x }
            base.Notify("Kind")

    member this.StockPrice
        with get() = userInput.StockPrice
        and set(x) = 
            userInput <- {userInput with StockPrice = x }
            base.Notify("StockPrice")

    member this.Strike
        with get() = userInput.Strike
        and set(x) = 
            userInput <- {userInput with Strike = x }
            base.Notify("Strike")

    member this.Expiry 
        with get() = userInput.Expiry
        and set(x) = 
            userInput <- {userInput with Expiry = x }
            base.Notify("Expiry")

    member this.r 
        with get() = userInput.r
        and set(x) = 
            userInput <- {userInput with r = x }
            base.Notify("r")

    member this.v
        with get() = userInput.v
        and set(x) = 
            userInput <- {userInput with v = x }
            base.Notify("v")


    // Invoke the valuation based on user input
    //member this.CalculateOption(data : DataConfiguration, calculationParameters : CalculationConfiguration) = 
        
    //    //capture inputs
    //    let paymentInputs : PaymentValuationInputs = 
    //        {
    //            Trade = 
    //                     {
    //                         TradeName = this.TradeName
    //                         Expiry    = this.Expiry
    //                         Currency  = this.Currency
    //                         Principal = this.Principal
    //                     }
    //            Data = data
    //            CalculationsParameters = calculationParameters
    //        }
    //    //calculate
    //    let calc = PaymentValuationModel(paymentInputs).Calculate()

    //    //present to the user
    //    this.Value <- Option.Some (calc)

(* summary row. there is little functionality here, so this is very brief. *)
//type SummaryRow = 
//    {
//        Currency: string
//        Value : float
//    }

//type OprionRecord = 
//    {
//        Key : string
//        Value : string
//    }
    
//type DataConfiguration = Map<string, string>
//type CalculationConfiguration = Map<string, string>

//type ConfigurationViewModel( configRec : ConfigurationRecord) = 
//    inherit ViewModelBase()

//    let mutable configRec = configRec

//    member this.Value
//        with get() = configRec.Value
//        and set(x) = 
//            configRec <- {configRec with Value = x }
//            base.Notify("Value")
    
//    member this.Key
//        with get() = configRec.Key
//        and set(x) = 
//            configRec <- {configRec with Key = x }
//            base.Notify("Key") 


