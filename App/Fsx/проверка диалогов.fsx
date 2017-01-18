let b = new Button(Parent = STM30.MainWindow.form, Location = Point(10,10), Text = "Samp1")
    let dlg = STM30.ViewModels.Thread2.modalMessageDialog
    dlg.SetParent ( STM30.MainWindow.form :> Control)
    b.BringToFront()
    b.Click.Add <| fun _ ->
        async{ 
            do!  dlg.ShowMessage Logging.Warn someLongText }
        |> Async.Start

    let b = new Button(Parent = STM30.MainWindow.form, Location = Point(10,50), Text = "Samp2")    
    b.BringToFront()
    b.Click.Add <| fun _ ->
        
        let textBox = new TextBox()
        let getResult() = 
            let b,v = Int32.TryParse textBox.Text
            if b then Some v else None

        let dlg = 
            { MyWinForms.ModalDialog.Options<_>.createNew getResult with
                Width = 300 
                Content = textBox
                Text = someLongText
                ButtonAcceptText = "Применить"
                ButtonCancel = Some "Отменить" }
            |> MyWinForms.ModalDialog.create STM30.MainWindow.form

        textBox.TextChanged.Add <| fun _ ->
            dlg.Validate()

        async{ 
            let! r =  dlg.Show someLongText 
            Logging.debug "modal dialog %A" r }
        |> Async.Start

    let b = new Button(Parent = STM30.MainWindow.form, Location = Point(10,100), Text = "Samp3")    
    b.BringToFront()
    b.Click.Add <| fun _ ->
        
        let dlg = 
            { MyWinForms.ModalDialog.Options<_>.createNew (fun () -> Some () ) with
                Width = 500 
                Text = someLongText
                ButtonAcceptText = "Продолжить"
                ButtonCancel = None }
            |> MyWinForms.ModalDialog.create STM30.MainWindow.form

        async{ 
            let! r =  dlg.Show someLongText 
            Logging.debug "modal dialog %A" r }
        |> Async.Start