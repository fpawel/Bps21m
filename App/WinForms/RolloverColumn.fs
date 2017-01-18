namespace MyWinForms

open System
open System.Drawing
open System.Windows.Forms
open System.Windows.Forms.VisualStyles
open System.ComponentModel

type DataGridViewRolloverCell() =
    inherit DataGridViewTextBoxCell()

    override this.Paint(    graphics,
                            clipBounds,
                            cellBounds,
                            rowIndex,
                            cellState,
                            value,
                            formattedValue,
                            errorText,
                            cellStyle,
                            advancedBorderStyle,
                            paintParts) = 
        // Call the base class method to paint the default cell appearance.
        base.Paint(graphics, clipBounds, cellBounds, rowIndex, cellState,
            value, formattedValue, errorText, cellStyle,
            advancedBorderStyle, paintParts);

        // Retrieve the client location of the mouse pointer.
        let cursorPosition =
            this.DataGridView.PointToClient(Cursor.Position)

        // If the mouse pointer is over the current cell, draw a custom border.
        if cellBounds.Contains(cursorPosition) then        
            let newRect = 
                new Rectangle
                    (   cellBounds.X + 1,
                        cellBounds.Y + 1, cellBounds.Width - 4,
                        cellBounds.Height - 4);
            
            graphics.DrawRectangle(Pens.Red, newRect);

    // Force the cell to repaint itself when the mouse pointer enters it.
    override this.OnMouseEnter(rowIndex) =    
        this.DataGridView.InvalidateCell(this)
    

    // Force the cell to repaint itself when the mouse pointer leaves it.
    override this.OnMouseLeave(rowIndex) =
        this.DataGridView.InvalidateCell(this)

type DataGridViewRolloverCellColumn() =
    inherit DataGridViewColumn()
    do
        base.CellTemplate <- new DataGridViewRolloverCell()
        
