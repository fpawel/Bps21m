module Main

open System
open System.Windows.Forms
open System.Drawing
//open FSharp.Reflection

let initializeMainForm =
    let safe = MyWinForms.Utils.safe
    let config = STM30.Config.config
    let party = STM30.ViewModels.Party.party
    let form = STM30.MainWindow.form
    
    STM30.View.Controls1.initialize
    STM30.View.Thread2.initialize
    STM30.View.PartyMainGrid.initialize
    STM30.View.PartyVarGrids.initialize
    STM30.View.Menus1.initialize


let handleError(e:Exception) = 
    Logging.error "%A" e 
    MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) |> ignore    
    

    


