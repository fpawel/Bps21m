open System
open System.Reflection
open System.Reflection.Emit

type private Ta = TypeAttributes
type private Ma = MethodAttributes
type private Opc = OpCodes 

let getTypeBuilder() : TypeBuilder =        
    let typeSignature = "MyDynamicType"
    let an = AssemblyName(typeSignature)
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(an, AssemblyBuilderAccess.Run);
    let moduleBuilder = assemblyBuilder.DefineDynamicModule("MainModule")    
    moduleBuilder.DefineType
        (typeSignature, 
            Ta.Public |||
            Ta.Class |||
            Ta.AutoClass |||
            Ta.AnsiClass |||
            Ta.BeforeFieldInit |||
            Ta.AutoLayout, null)
    

let createProperty(tb:TypeBuilder) (propertyName:string) (propertyType:Type ) =        
    let fieldBuilder = tb.DefineField("_" + propertyName, propertyType, FieldAttributes.Private);
    let propertyBuilder = tb.DefineProperty(propertyName, PropertyAttributes.HasDefault, propertyType, null);
    let getPropMthdBldr = 
        tb.DefineMethod
            (   "get_" + propertyName, 
                Ma.Public ||| Ma.SpecialName ||| Ma.HideBySig, 
                propertyType, Type.EmptyTypes);
    let getIl = getPropMthdBldr.GetILGenerator()

    getIl.Emit(Opc.Ldarg_0);
    getIl.Emit(Opc.Ldfld, fieldBuilder);
    getIl.Emit(Opc.Ret);

    let setPropMthdBldr =
        tb.DefineMethod
            (   "set_" + propertyName,
                Ma.Public ||| Ma.SpecialName ||| Ma.HideBySig,
                null, [| propertyType |] );

    let setIl = setPropMthdBldr.GetILGenerator();
    let modifyProperty = setIl.DefineLabel();
    let exitSet = setIl.DefineLabel();

    setIl.MarkLabel(modifyProperty);
    setIl.Emit(OpCodes.Ldarg_0);
    setIl.Emit(OpCodes.Ldarg_1);
    setIl.Emit(OpCodes.Stfld, fieldBuilder);

    setIl.Emit(OpCodes.Nop);
    setIl.MarkLabel(exitSet);
    setIl.Emit(OpCodes.Ret);

    propertyBuilder.SetGetMethod(getPropMthdBldr);
    propertyBuilder.SetSetMethod(setPropMthdBldr);
        
        

let compileResultType fields : Type =        
    let tb = getTypeBuilder();
    let constructor' = 
        tb.DefineDefaultConstructor
            (   MethodAttributes.Public ||| 
                MethodAttributes.SpecialName ||| 
                MethodAttributes.RTSpecialName)

    // NOTE: assuming your list contains Field objects with fields FieldName(string) and FieldType(Type)
    for fieldName,fieldType in fields do
        createProperty tb fieldName fieldType

    tb.CreateType()
