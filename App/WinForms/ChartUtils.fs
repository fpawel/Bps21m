module MyWinForms.ChartUtils

open System
open System.Windows.Forms.DataVisualization.Charting

let erraseVisiblePoints( chart : Chart) =    
    let ar = chart.ChartAreas.[0]
    let xAx = ar.AxisX
    let yAx = ar.AxisY
    let xmin = Math.Max(xAx.Minimum, xAx.ScaleView.ViewMinimum)
    let xmax = Math.Min(xAx.Maximum, xAx.ScaleView.ViewMaximum)
    let ymin = Math.Max(yAx.Minimum, yAx.ScaleView.ViewMinimum)
    let ymax = Math.Min(yAx.Maximum, yAx.ScaleView.ViewMaximum)    
    [   for series in chart.Series do                
            for pt in series.Points do        
                let x = pt.XValue
                let y = pt.YValues.[0]                    
                if x >= xmin && x <= xmax && y >= ymin && y <= ymax then
                    yield series, pt  ]
    |> List.iter(fun (series,pt) -> series.Points.Remove pt |> ignore)
    xAx.Minimum <- Double.NaN
    xAx.Maximum <- Double.NaN
    yAx.Minimum <- Double.NaN
    yAx.Maximum <- Double.NaN