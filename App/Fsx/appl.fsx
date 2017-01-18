#I "../../packages"
#I "../../packages/FParsec.1.0.2/lib/net40-client/"
#r "FParsec.dll"
#r "FParsecCS.dll"

#I "../../packages/FsPickler.2.3.0/lib/net40/"
#r "FsPickler.dll"

#I "../../NumericMethod/bin/Release"
#r "NumericMethod.dll"

#I "../../MyWinForms/bin/Release"
#r "MyWinForms.dll"
#I "../../Widgets/bin/Release"
#r "Widgets.dll"

#r "System.Windows.Forms.dll"
#r "System.Windows.Forms.DataVisualization.dll"

#load "../Utils/FsharpRuntime.fs"
#load "../Utils/State.fs"

#load "../Utils/StrUtils.fs"
#load "../Utils/PathUtils.fs"
#load "../Utils/DateTimeUtils.fs"
#load "../Utils/Logging.fs"
#load "../Utils/Utils.fs"
#load "../Bps21/Coef.fs"
#load "../Bps21/ProductType.fs"
#load "../Bps21/Bps21.fs"

#load "../Utils/Utils.fs" 

open StrUtils

#load "../Utils/Ref.fs" 
#load "../Utils/Tree.fs" 
#load "../Utils/Html.fs" 
#load "../Utils/Runtime.fs" 
#load "../Utils/Bin.fs" 
#load "../Utils/Hex.fs" 
#load "../Utils/IntRanges.fs"
#load "../WinForms/WinFormsControlUtils.fs" 
#load "../WinForms/WinFormsConverters.fs" 
#load "../WinForms/WinFormsUtils.fs"
#load "../WinForms/ChartUtils.fs"

#load "../WinForms/Components/PopupDialog.fs"
#load "../WinForms/Components/TopDialog.fs"
#load "../WinForms/Components/TriStateCheckTreeView.fs"
#load "../WinForms/Components/LeftInfoBlock.fs" 
#load "../WinForms/Components/ChartAxisScalingViewModel.fs" 

#load "../Json/Json.fs" 
#load "../Json/JsonSerialization.fs" 
#load "../Json/JsonConfig.fs" 
#load "../AppCore/ComportConfig.fs" 
#load "../AppCore/Comport.fs" 
#load "../AppCore/AppConfig.fs" 
#load "../AppCore/MainWindow.fs" 
#load "../AppCore/Mdbs.fs" 
#load "../AppCore/Hardware.fs" 
#load "../AppCore/Repository.fs" 
#load "../AppCore/PhysVarValues.fs" 
#load "../AppCore/Chart.fs" 
#load "../AppCore/Alchemy.fs" 
#load "../ViewModels/ViewModel.fs" 
#load "../ViewModels/ProductViewModel1.fs" 
#load "../ViewModels/ProductViewModel.fs" 
#load "../ViewModels/SelectPhysVarsViewModel.fs" 
#load "../ViewModels/AppConfigViewModel.fs" 
#load "../ViewModels/PartyViewModel1.fs" 
#load "../ViewModels/PartyViewModel.fs" 
#load  "../Work/AppContent.fs" 

#load  "../Work/Operation.fs" 
#load  "../Work/Thread2.fs" 
#load  "../Work/PartyWorks.fs" 
#load  "../View/LoggingHtml.fs" 
#load  "../View/TopBar.fs" 
#load  "../View/ViewProducts.fs" 
#load  "../View/TabPages.fs" 
#load  "../View/DelaysHelpViewModel1.fs" 
#load  "../View/DelaysHelpViewModel.fs" 
#load  "../View/ViewScenary.fs" 
#load  "../View/Thread2Views.fs" 
#load  "../View/ReportView.fs" 
#load  "../View/ViewChart.fs" 
#load  "../View/Menus1.fs" 

open System
open System.IO
open System.Windows.Forms
open System.Threading

open Bps21.View
open Bps21.ViewModel

let main () = 
    try 
        Application.EnableVisualStyles() 
        Application.SetCompatibleTextRenderingDefault true 
        
        let config = AppConfig.config
        let party = Bps21.AppContent.party
        let form = MainWindow.form

        MainWindow.initialize()
        TabPages.setSelected MainWindow.TabsheetParty
        TopBar.initialize()
        Products.initialize()        
        Scenary.initialize()
        Thread2.initialize()
        ChartDataBindings.initialize()
        Report.initialize()
        Menus1.initialize()
        Ref.``check referencies was initialized``()
        
        Application.Run MainWindow.form 
        Bps21.AppContent.save()
        
        
    with e ->
//        Logging.error "%A" e 
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) |> ignore   
    AppConfig.save()


    
main()