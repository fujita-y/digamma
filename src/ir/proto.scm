(push_loc
  (one = getInt32(1))
  (x = getArg(0))
  (add = createAdd(one, x))
  (createRet(add)))

void emit_foobar(Function& F, IRBuilder<>& B)
{
    auto one = B.getInt32(1);
    auto x = F.arg_begin() + 0; // F.getArg(0);
    auto add = B.CreateAdd(one, x);
    B.CreateRet(add);
}

void emit_push_iloc(Function& F, IRBuilder<>& B)
{
    auto m_vm = F.arg_begin() + 0;
    ...
}
