namespace Mil82.ViewModel
open System
open System.ComponentModel
open Mil82

type InterrogateConverter() =
    inherit MyWinForms.Converters.BooleanTypeConverter("Опрашивать", "Не опрашивать")

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type SelectPhysVars = {
[<DisplayName("Конц.")>]
    [<Description("Концентрация")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Conc : string }
[<DisplayName("Iизл")>]
    [<Description("Ток излучателя")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Curr : string }
[<DisplayName("Var1")>]
    [<Description("Var1")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Var1 : string }
[<DisplayName("Т"С")>]
    [<Description("Температура")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Temp : string }
[<DisplayName("Uраб")>]
    [<Description("Рабочий канал")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Workk : string }
[<DisplayName("Uоп")>]
    [<Description("Опорный канал")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    mutable Refk : string }
