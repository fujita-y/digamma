; Function *add1 = Function::Create(
;                        FunctionType::get(retType, argTypes, false),
;                        Function::ExternalLinkage,
;                        "add1",
;                        M.get())

(function add1 (i32 (i32) false ExternalLinkage)
    (One = getInt32(1))         ; Value* One = builder.getInt32(1);
    (ArgX = getArg(0))          ; Argument *ArgX = &*add1->getArg(0); ArgX->setName("ArgX");
    (Add = createAdd(One Argx)) ; Value *Add = builder.CreateAdd(One, ArgX);
    (createRet(Add)))           ; builder.CreateRet(Add)
