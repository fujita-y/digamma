;*** IR after optimize ***
; ModuleID = '4e3c5774-386e-42e6-9c8d-7ee12a1eae30'
source_filename = "4e3c5774-386e-42e6-9c8d-7ee12a1eae30"

define i64 @"8fb2aece-298c-423e-884a-5512a5083426"(i64* noalias nocapture %0) local_unnamed_addr {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 88
  %6 = icmp ult i64 %5, %2
  br i1 %6, label %entry.stack_ok_crit_edge, label %stack_overflow, !prof !0

entry.stack_ok_crit_edge:                         ; preds = %entry
  %7 = inttoptr i64 %4 to i64*
  br label %stack_ok

stack_ok:                                         ; preds = %entry.stack_ok_crit_edge, %stack_overflow
  %8 = phi i64* [ %.pre, %stack_overflow ], [ %7, %entry.stack_ok_crit_edge ]
  store i64 4635770064, i64* %8, align 4
  %9 = getelementptr i64, i64* %8, i64 1
  store i64 82, i64* %9, align 4
  %10 = getelementptr i64, i64* %0, i64 2
  %11 = load i64, i64* %10, align 4
  %12 = getelementptr i64, i64* %8, i64 2
  store i64 %11, i64* %12, align 4
  %13 = getelementptr i64, i64* %0, i64 3
  %14 = load i64, i64* %13, align 4
  %15 = getelementptr i64, i64* %8, i64 3
  store i64 %14, i64* %15, align 4
  %16 = getelementptr i64, i64* %8, i64 4
  store i64 ptrtoint (i64 (i64*)* @c774198f-eb0c-4775-8000-7e4d93de65c0 to i64), i64* %16, align 4
  %17 = getelementptr i64, i64* %0, i64 4
  %18 = load i64, i64* %17, align 4
  %19 = getelementptr i64, i64* %8, i64 5
  store i64 %18, i64* %19, align 4
  %20 = getelementptr i64, i64* %8, i64 6
  %21 = ptrtoint i64* %19 to i64
  %22 = inttoptr i64 %14 to i64*
  %23 = getelementptr i64, i64* %22, i64 2305843009213693950
  %24 = load i64, i64* %23, align 4
  store i64 %24, i64* %20, align 4
  %25 = getelementptr i64, i64* %8, i64 7
  store i64 1, i64* %25, align 4
  %26 = getelementptr i64, i64* %8, i64 8
  store i64 0, i64* %26, align 4
  %27 = getelementptr i64, i64* %8, i64 9
  %28 = ptrtoint i64* %27 to i64
  %29 = ptrtoint i64* %26 to i64
  store i64 4635844976, i64* %0, align 4
  store i64 %28, i64* %10, align 4
  store i64 %29, i64* %13, align 4
  store i64 %21, i64* %17, align 4
  store i64 %28, i64* %3, align 4
  %30 = musttail call i64 @"482ea6ea-0a04-4860-9252-5ec2c53905d4"(i64* nonnull %0)
  ret i64 %30

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 88)
  %.phi.trans.insert = bitcast i64* %3 to i64**
  %.pre = load i64*, i64** %.phi.trans.insert, align 4
  br label %stack_ok
}

declare void @c_collect_stack(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @c774198f-eb0c-4775-8000-7e4d93de65c0(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = bitcast i64* %3 to i64**
  %5 = load i64*, i64** %4, align 4
  store i64 %2, i64* %5, align 4
  %6 = getelementptr i64, i64* %5, i64 1
  store i64 50, i64* %6, align 4
  %7 = getelementptr i64, i64* %5, i64 2
  store i64 50, i64* %7, align 4
  %8 = getelementptr i64, i64* %5, i64 3
  store i64 3, i64* %8, align 4
  %9 = getelementptr i64, i64* %5, i64 4
  store i64 0, i64* %9, align 4
  %10 = getelementptr i64, i64* %5, i64 5
  %11 = ptrtoint i64* %10 to i64
  %12 = ptrtoint i64* %9 to i64
  store i64 4635848480, i64* %0, align 4
  %13 = getelementptr i64, i64* %0, i64 2
  store i64 %11, i64* %13, align 4
  %14 = getelementptr i64, i64* %0, i64 3
  store i64 %12, i64* %14, align 4
  store i64 %11, i64* %3, align 4
  %15 = musttail call i64 @"5796dc35-449b-4518-8560-d1c4ae1c07ee"(i64* nonnull %0)
  ret i64 %15
}

; Function Attrs: nounwind
define i64 @"482ea6ea-0a04-4860-9252-5ec2c53905d4"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 32
  %6 = icmp ult i64 %5, %2
  br i1 %6, label %entry.stack_ok_crit_edge, label %stack_overflow, !prof !0

entry.stack_ok_crit_edge:                         ; preds = %entry
  %7 = inttoptr i64 %4 to i64*
  br label %stack_ok

stack_ok:                                         ; preds = %entry.stack_ok_crit_edge, %stack_overflow
  %8 = phi i64* [ %.pre, %stack_overflow ], [ %7, %entry.stack_ok_crit_edge ]
  %9 = getelementptr i64, i64* %0, i64 3
  %10 = bitcast i64* %9 to i64**
  %11 = load i64*, i64** %10, align 4
  %12 = getelementptr i64, i64* %11, i64 2305843009213693950
  %13 = load i64, i64* %12, align 4
  store i64 %13, i64* %8, align 4
  %14 = getelementptr i64, i64* %8, i64 1
  store i64 50, i64* %14, align 4
  %15 = getelementptr i64, i64* %8, i64 2
  store i64 2, i64* %15, align 4
  %16 = getelementptr i64, i64* %8, i64 3
  store i64 0, i64* %16, align 4
  %17 = getelementptr i64, i64* %8, i64 4
  %18 = ptrtoint i64* %17 to i64
  %19 = ptrtoint i64* %16 to i64
  store i64 4635851952, i64* %0, align 4
  %20 = getelementptr i64, i64* %0, i64 2
  store i64 %18, i64* %20, align 4
  store i64 %19, i64* %9, align 4
  store i64 %18, i64* %3, align 4
  %21 = musttail call i64 @"03c70879-9207-4716-9682-b22d5eaf7ea2"(i64* nonnull %0)
  ret i64 %21

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  %.phi.trans.insert = bitcast i64* %3 to i64**
  %.pre = load i64*, i64** %.phi.trans.insert, align 4
  br label %stack_ok
}

; Function Attrs: nounwind
define i64 @"03c70879-9207-4716-9682-b22d5eaf7ea2"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = getelementptr i64, i64* %0, i64 6
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = bitcast i64* %3 to i64**
  %5 = bitcast i64* %2 to i64**
  %6 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %2, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %continue1, %entry
  %7 = phi i64 [ %31, %continue1 ], [ %.pre, %entry ]
  %8 = load i64, i64* %1, align 4
  %9 = add i64 %7, 32
  %10 = icmp ult i64 %9, %8
  br i1 %10, label %stack_ok, label %stack_overflow, !prof !0

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %11 = load i64*, i64** %4, align 4
  %12 = getelementptr i64, i64* %11, i64 2305843009213693949
  %13 = load i64, i64* %12, align 4
  %14 = and i64 %13, 1
  %15 = icmp eq i64 %14, 0
  br i1 %15, label %nonfixnum_true, label %nonfixnum_false, !prof !1

stack_overflow:                                   ; preds = %tailrecurse
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  br label %stack_ok

nonfixnum_true:                                   ; preds = %stack_ok
  store i64 4635851952, i64* %0, align 4
  %16 = tail call i64 @c_eq_n_iloc(i64* nonnull %0, i64 %13, i64 1) #0
  switch i64 %16, label %f9h_false [
    i64 0, label %fallback_fail
    i64 34, label %f9h_true
  ], !prof !2

nonfixnum_false:                                  ; preds = %stack_ok
  %17 = icmp eq i64 %13, 1
  br i1 %17, label %f9h_false, label %fixnum_true, !prof !1

fallback_fail:                                    ; preds = %nonfixnum_true, %fallback
  ret i64 3

f9h_true:                                         ; preds = %nonfixnum_true
  %.pre10 = load i64, i64* %12, align 4
  %.pre11 = and i64 %.pre10, 1
  %18 = icmp eq i64 %.pre11, 0
  br i1 %18, label %fallback, label %fixnum_true

f9h_false:                                        ; preds = %nonfixnum_false, %nonfixnum_true
  %19 = getelementptr i64, i64* %11, i64 2305843009213693950
  %20 = load i64, i64* %19, align 4
  %21 = getelementptr i64, i64* %0, i64 5
  store i64 %20, i64* %21, align 4
  ret i64 1

continue1:                                        ; preds = %fallback, %valid_true
  %.05 = phi i64 [ %38, %valid_true ], [ %36, %fallback ]
  %22 = load i64*, i64** %5, align 4
  store i64 %.05, i64* %22, align 4
  %23 = getelementptr i64, i64* %22, i64 1
  %24 = load i64, i64* %12, align 4
  store i64 %24, i64* %23, align 4
  %25 = getelementptr i64, i64* %22, i64 2
  %26 = getelementptr i64, i64* %11, i64 2305843009213693950
  %27 = load i64, i64* %26, align 4
  %28 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %24, i64 %27) #0
  store i64 %28, i64* %23, align 4
  store i64 2, i64* %25, align 4
  %29 = getelementptr i64, i64* %22, i64 3
  store i64 0, i64* %29, align 4
  %30 = getelementptr i64, i64* %22, i64 4
  %31 = ptrtoint i64* %30 to i64
  %32 = ptrtoint i64* %29 to i64
  store i64 4635851952, i64* %0, align 4
  store i64 %31, i64* %6, align 4
  store i64 %32, i64* %3, align 4
  store i64 %31, i64* %2, align 4
  br label %tailrecurse

fixnum_true:                                      ; preds = %nonfixnum_false, %f9h_true
  %33 = phi i64 [ %.pre10, %f9h_true ], [ %13, %nonfixnum_false ]
  %34 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %33, i64 -2)
  %35 = extractvalue { i64, i1 } %34, 1
  br i1 %35, label %fallback, label %valid_true, !prof !1

fallback:                                         ; preds = %fixnum_true, %f9h_true
  %36 = tail call i64 @c_nadd_iloc(i64* nonnull %0, i64 4635852544) #0
  %37 = icmp eq i64 %36, 0
  br i1 %37, label %fallback_fail, label %continue1, !prof !1

valid_true:                                       ; preds = %fixnum_true
  %38 = extractvalue { i64, i1 } %34, 0
  br label %continue1
}

declare i64 @c_eq_n_iloc(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i64 @c_nadd_iloc(i64*, i64) local_unnamed_addr

declare i64 @c_make_pair(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"5796dc35-449b-4518-8560-d1c4ae1c07ee"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 152
  %6 = icmp ult i64 %5, %2
  br i1 %6, label %stack_ok, label %stack_overflow, !prof !0

stack_ok:                                         ; preds = %stack_overflow, %entry
  %7 = getelementptr i64, i64* %0, i64 3
  %8 = load i64, i64* %7, align 4
  %9 = inttoptr i64 %8 to i64*
  %10 = getelementptr i64, i64* %9, i64 2305843009213693948
  %11 = load i64, i64* %10, align 4
  %12 = icmp eq i64 %11, 50
  br i1 %12, label %taken_true, label %taken_false, !prof !1

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 152) #0
  br label %stack_ok

taken_true:                                       ; preds = %stack_ok
  %13 = getelementptr i64, i64* %9, i64 2305843009213693949
  %14 = load i64, i64* %13, align 4
  %15 = icmp eq i64 %14, 50
  %16 = getelementptr i64, i64* %0, i64 5
  br i1 %15, label %taken_true1, label %taken_false2, !prof !1

taken_false:                                      ; preds = %stack_ok
  %17 = bitcast i64* %3 to i64**
  %18 = load i64*, i64** %17, align 4
  store i64 4635848432, i64* %18, align 4
  %19 = getelementptr i64, i64* %18, i64 1
  store i64 82, i64* %19, align 4
  %20 = getelementptr i64, i64* %0, i64 2
  %21 = load i64, i64* %20, align 4
  %22 = getelementptr i64, i64* %18, i64 2
  store i64 %21, i64* %22, align 4
  %23 = getelementptr i64, i64* %18, i64 3
  store i64 %8, i64* %23, align 4
  %24 = getelementptr i64, i64* %18, i64 4
  store i64 ptrtoint (i64 (i64*)* @"003d62b3-9080-4271-9955-57819026bc0f" to i64), i64* %24, align 4
  %25 = getelementptr i64, i64* %0, i64 4
  %26 = load i64, i64* %25, align 4
  %27 = getelementptr i64, i64* %18, i64 5
  store i64 %26, i64* %27, align 4
  %28 = getelementptr i64, i64* %18, i64 6
  %29 = ptrtoint i64* %28 to i64
  %30 = ptrtoint i64* %27 to i64
  store i64 4635849712, i64* %28, align 4
  %31 = getelementptr i64, i64* %18, i64 7
  store i64 82, i64* %31, align 4
  %32 = getelementptr i64, i64* %18, i64 8
  store i64 %29, i64* %32, align 4
  %33 = getelementptr i64, i64* %18, i64 9
  store i64 %8, i64* %33, align 4
  %34 = getelementptr i64, i64* %18, i64 10
  store i64 ptrtoint (i64 (i64*)* @"44ac696d-0a2f-4be5-850f-7815bab27706" to i64), i64* %34, align 4
  %35 = getelementptr i64, i64* %18, i64 11
  store i64 %30, i64* %35, align 4
  %36 = getelementptr i64, i64* %18, i64 12
  %37 = ptrtoint i64* %36 to i64
  %38 = ptrtoint i64* %35 to i64
  %39 = load i64, i64* %10, align 4
  %40 = and i64 %39, 7
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %cond1_true, label %pair_false, !prof !0

taken_true1:                                      ; preds = %taken_true
  store i64 3, i64* %16, align 4
  ret i64 1

taken_false2:                                     ; preds = %taken_true
  store i64 1, i64* %16, align 4
  ret i64 1

pair_true:                                        ; preds = %cond1_true
  store i64 %54, i64* %36, align 4
  %42 = getelementptr i64, i64* %18, i64 13
  store i64 3, i64* %42, align 4
  %43 = getelementptr i64, i64* %18, i64 14
  %44 = getelementptr i64, i64* %9, i64 2305843009213693950
  %45 = load i64, i64* %44, align 4
  store i64 %45, i64* %43, align 4
  %46 = getelementptr i64, i64* %18, i64 15
  store i64 3, i64* %46, align 4
  %47 = getelementptr i64, i64* %18, i64 16
  store i64 0, i64* %47, align 4
  %48 = getelementptr i64, i64* %18, i64 17
  %49 = ptrtoint i64* %48 to i64
  %50 = ptrtoint i64* %47 to i64
  store i64 4635845856, i64* %0, align 4
  store i64 %49, i64* %20, align 4
  store i64 %50, i64* %7, align 4
  store i64 %38, i64* %25, align 4
  store i64 %49, i64* %3, align 4
  %51 = musttail call i64 @"86821bb4-da31-4486-8544-d06da7fb5300"(i64* nonnull %0)
  ret i64 %51

pair_false:                                       ; preds = %cond1_true, %taken_false
  store i64 %37, i64* %20, align 4
  store i64 %38, i64* %25, align 4
  store i64 %37, i64* %3, align 4
  %52 = getelementptr i64, i64* %0, i64 5
  store i64 %11, i64* %52, align 4
  store i64 4635850896, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %39) #0
  ret i64 3

cond1_true:                                       ; preds = %taken_false
  %53 = inttoptr i64 %39 to i64*
  %54 = load i64, i64* %53, align 4
  %55 = and i64 %54, 15
  %56 = icmp eq i64 %55, 10
  br i1 %56, label %pair_false, label %pair_true, !prof !1
}

; Function Attrs: nounwind
define i64 @"003d62b3-9080-4271-9955-57819026bc0f"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = bitcast i64* %3 to i64**
  %5 = load i64*, i64** %4, align 4
  store i64 %2, i64* %5, align 4
  %6 = getelementptr i64, i64* %5, i64 1
  store i64 4635848400, i64* %6, align 4
  %7 = getelementptr i64, i64* %5, i64 2
  store i64 82, i64* %7, align 4
  %8 = getelementptr i64, i64* %0, i64 2
  %9 = load i64, i64* %8, align 4
  %10 = getelementptr i64, i64* %5, i64 3
  store i64 %9, i64* %10, align 4
  %11 = getelementptr i64, i64* %0, i64 3
  %12 = load i64, i64* %11, align 4
  %13 = getelementptr i64, i64* %5, i64 4
  store i64 %12, i64* %13, align 4
  %14 = getelementptr i64, i64* %5, i64 5
  store i64 ptrtoint (i64 (i64*)* @"3f297490-673d-43f4-8aad-524975092d89" to i64), i64* %14, align 4
  %15 = getelementptr i64, i64* %0, i64 4
  %16 = load i64, i64* %15, align 4
  %17 = getelementptr i64, i64* %5, i64 6
  store i64 %16, i64* %17, align 4
  %18 = getelementptr i64, i64* %5, i64 7
  %19 = ptrtoint i64* %18 to i64
  %20 = ptrtoint i64* %17 to i64
  %21 = inttoptr i64 %12 to i64*
  %22 = getelementptr i64, i64* %21, i64 2305843009213693948
  %23 = load i64, i64* %22, align 4
  %24 = and i64 %23, 7
  %25 = icmp eq i64 %24, 0
  br i1 %25, label %cond1_true, label %pair_false, !prof !0

pair_true:                                        ; preds = %cond1_true
  %26 = getelementptr i64, i64* %33, i64 1
  %27 = load i64, i64* %26, align 4
  store i64 %27, i64* %18, align 4
  %28 = getelementptr i64, i64* %5, i64 8
  %29 = ptrtoint i64* %28 to i64
  %30 = load i64, i64* %22, align 4
  %31 = and i64 %30, 7
  %32 = icmp eq i64 %31, 0
  br i1 %32, label %cond1_true3, label %pair_false2, !prof !0

pair_false:                                       ; preds = %cond1_true, %entry
  store i64 %19, i64* %8, align 4
  store i64 %20, i64* %15, align 4
  store i64 %19, i64* %3, align 4
  store i64 4635848912, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %23) #0
  ret i64 3

cond1_true:                                       ; preds = %entry
  %33 = inttoptr i64 %23 to i64*
  %34 = load i64, i64* %33, align 4
  %35 = and i64 %34, 15
  %36 = icmp eq i64 %35, 10
  br i1 %36, label %pair_false, label %pair_true, !prof !1

pair_true1:                                       ; preds = %cond1_true3
  store i64 %50, i64* %28, align 4
  %37 = getelementptr i64, i64* %5, i64 9
  %38 = getelementptr i64, i64* %21, i64 2305843009213693949
  %39 = load i64, i64* %38, align 4
  %40 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %50, i64 %39) #0
  store i64 %40, i64* %28, align 4
  %41 = getelementptr i64, i64* %21, i64 2305843009213693950
  %42 = load i64, i64* %41, align 4
  store i64 %42, i64* %37, align 4
  %43 = getelementptr i64, i64* %5, i64 10
  store i64 3, i64* %43, align 4
  %44 = getelementptr i64, i64* %5, i64 11
  store i64 0, i64* %44, align 4
  %45 = getelementptr i64, i64* %5, i64 12
  %46 = ptrtoint i64* %45 to i64
  %47 = ptrtoint i64* %44 to i64
  store i64 4635848480, i64* %0, align 4
  store i64 %46, i64* %8, align 4
  store i64 %47, i64* %11, align 4
  store i64 %20, i64* %15, align 4
  store i64 %46, i64* %3, align 4
  %48 = musttail call i64 @"5796dc35-449b-4518-8560-d1c4ae1c07ee"(i64* nonnull %0)
  ret i64 %48

pair_false2:                                      ; preds = %cond1_true3, %pair_true
  store i64 %19, i64* %8, align 4
  store i64 %20, i64* %15, align 4
  store i64 %29, i64* %3, align 4
  store i64 4635848896, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %30) #0
  ret i64 3

cond1_true3:                                      ; preds = %pair_true
  %49 = inttoptr i64 %30 to i64*
  %50 = load i64, i64* %49, align 4
  %51 = and i64 %50, 15
  %52 = icmp eq i64 %51, 10
  br i1 %52, label %pair_false2, label %pair_true1, !prof !1
}

; Function Attrs: nounwind
define i64 @"44ac696d-0a2f-4be5-850f-7815bab27706"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = icmp eq i64 %2, 34
  br i1 %3, label %f9h_true, label %f9h_false, !prof !1

f9h_true:                                         ; preds = %entry
  store i64 1, i64* %1, align 4
  ret i64 1

f9h_false:                                        ; preds = %entry
  %4 = getelementptr i64, i64* %0, i64 3
  %5 = bitcast i64* %4 to i64**
  %6 = load i64*, i64** %5, align 4
  %7 = getelementptr i64, i64* %6, i64 2305843009213693948
  %8 = load i64, i64* %7, align 4
  %9 = and i64 %8, 7
  %10 = icmp eq i64 %9, 0
  br i1 %10, label %cond1_true, label %pair_false, !prof !0

pair_true:                                        ; preds = %cond1_true
  %11 = getelementptr i64, i64* %21, i64 1
  %12 = load i64, i64* %11, align 4
  %13 = getelementptr i64, i64* %0, i64 6
  %14 = bitcast i64* %13 to i64**
  %15 = load i64*, i64** %14, align 4
  store i64 %12, i64* %15, align 4
  %16 = getelementptr i64, i64* %15, i64 1
  %17 = getelementptr i64, i64* %6, i64 2305843009213693949
  %18 = load i64, i64* %17, align 4
  store i64 %18, i64* %16, align 4
  store i64 4635849824, i64* %0, align 4
  %19 = tail call i64 inttoptr (i64 4532076672 to i64 (i64*, i64, i64*)*)(i64* nonnull %0, i64 2, i64* nonnull %15) #0
  store i64 %19, i64* %15, align 4
  %20 = icmp eq i64 %19, 66
  br i1 %20, label %undef_true, label %continue, !prof !1

pair_false:                                       ; preds = %cond1_true, %f9h_false
  store i64 4635849856, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %8) #0
  ret i64 3

cond1_true:                                       ; preds = %f9h_false
  %21 = inttoptr i64 %8 to i64*
  %22 = load i64, i64* %21, align 4
  %23 = and i64 %22, 15
  %24 = icmp eq i64 %23, 10
  br i1 %24, label %pair_false, label %pair_true, !prof !1

continue:                                         ; preds = %pair_true
  store i64 50, i64* %16, align 4
  %25 = getelementptr i64, i64* %15, i64 2
  %26 = ptrtoint i64* %25 to i64
  %27 = load i64, i64* %7, align 4
  %28 = and i64 %27, 7
  %29 = icmp eq i64 %28, 0
  br i1 %29, label %cond1_true3, label %pair_false2, !prof !0

undef_true:                                       ; preds = %pair_true
  store i64 66, i64* %1, align 4
  ret i64 3

pair_true1:                                       ; preds = %cond1_true3
  store i64 %41, i64* %25, align 4
  %30 = getelementptr i64, i64* %15, i64 3
  %31 = getelementptr i64, i64* %6, i64 2305843009213693950
  %32 = load i64, i64* %31, align 4
  %33 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %41, i64 %32) #0
  store i64 %33, i64* %25, align 4
  store i64 3, i64* %30, align 4
  %34 = getelementptr i64, i64* %15, i64 4
  store i64 0, i64* %34, align 4
  %35 = getelementptr i64, i64* %15, i64 5
  %36 = ptrtoint i64* %35 to i64
  %37 = ptrtoint i64* %34 to i64
  store i64 4635848480, i64* %0, align 4
  %38 = getelementptr i64, i64* %0, i64 2
  store i64 %36, i64* %38, align 4
  store i64 %37, i64* %4, align 4
  store i64 %36, i64* %13, align 4
  %39 = musttail call i64 @"5796dc35-449b-4518-8560-d1c4ae1c07ee"(i64* nonnull %0)
  ret i64 %39

pair_false2:                                      ; preds = %cond1_true3, %continue
  store i64 %26, i64* %13, align 4
  store i64 %19, i64* %1, align 4
  store i64 4635849792, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %27) #0
  ret i64 3

cond1_true3:                                      ; preds = %continue
  %40 = inttoptr i64 %27 to i64*
  %41 = load i64, i64* %40, align 4
  %42 = and i64 %41, 15
  %43 = icmp eq i64 %42, 10
  br i1 %43, label %pair_false2, label %pair_true1, !prof !1
}

declare void @c_error_push_car_iloc(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"86821bb4-da31-4486-8544-d06da7fb5300"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = getelementptr i64, i64* %0, i64 6
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = bitcast i64* %3 to i64**
  %5 = bitcast i64* %2 to i64**
  %6 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %2, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %pair_true13, %entry
  %7 = phi i64 [ %71, %pair_true13 ], [ %.pre, %entry ]
  %8 = load i64, i64* %1, align 4
  %9 = add i64 %7, 104
  %10 = icmp ult i64 %9, %8
  br i1 %10, label %stack_ok, label %stack_overflow, !prof !0

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %11 = load i64*, i64** %4, align 4
  %12 = getelementptr i64, i64* %11, i64 2305843009213693950
  %13 = load i64, i64* %12, align 4
  %14 = icmp eq i64 %13, 50
  br i1 %14, label %taken_true, label %taken_false, !prof !1

stack_overflow:                                   ; preds = %tailrecurse
  tail call void @c_collect_stack(i64* nonnull %0, i64 104) #0
  br label %stack_ok

taken_true:                                       ; preds = %stack_ok
  %15 = getelementptr i64, i64* %0, i64 5
  store i64 18, i64* %15, align 4
  ret i64 1

taken_false:                                      ; preds = %stack_ok
  %16 = and i64 %13, 7
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %cond1_true, label %pair_false, !prof !0

pair_true:                                        ; preds = %cond1_true
  %18 = load i64*, i64** %5, align 4
  store i64 %29, i64* %18, align 4
  %19 = getelementptr i64, i64* %18, i64 1
  %20 = getelementptr i64, i64* %11, i64 2305843009213693948
  %21 = load i64, i64* %20, align 4
  store i64 %21, i64* %19, align 4
  %22 = getelementptr i64, i64* %18, i64 2
  %23 = getelementptr i64, i64* %11, i64 2305843009213693949
  %24 = load i64, i64* %23, align 4
  store i64 %24, i64* %22, align 4
  store i64 4635845776, i64* %0, align 4
  %25 = tail call i64 inttoptr (i64 4532119312 to i64 (i64*, i64, i64*)*)(i64* nonnull %0, i64 2, i64* nonnull %19) #0
  store i64 %25, i64* %19, align 4
  %26 = icmp eq i64 %25, 66
  br i1 %26, label %undef_true, label %continue, !prof !1

pair_false:                                       ; preds = %cond1_true, %taken_false
  %27 = getelementptr i64, i64* %0, i64 5
  store i64 18, i64* %27, align 4
  store i64 4635845824, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %13) #0
  ret i64 3

cond1_true:                                       ; preds = %taken_false
  %28 = inttoptr i64 %13 to i64*
  %29 = load i64, i64* %28, align 4
  %30 = and i64 %29, 15
  %31 = icmp eq i64 %30, 10
  br i1 %31, label %pair_false, label %pair_true, !prof !1

continue:                                         ; preds = %pair_true
  store i64 4635845760, i64* %0, align 4
  %32 = tail call i64 inttoptr (i64 4532114896 to i64 (i64*, i64, i64*)*)(i64* nonnull %0, i64 2, i64* nonnull %18) #0
  switch i64 %32, label %value_nonfalse [
    i64 66, label %undef_true2
    i64 34, label %value_false
  ], !prof !2

undef_true:                                       ; preds = %pair_true
  %33 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %33, align 4
  ret i64 3

undef_true2:                                      ; preds = %continue
  %34 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %34, align 4
  ret i64 3

value_false:                                      ; preds = %continue
  %35 = load i64, i64* %12, align 4
  %36 = and i64 %35, 7
  %37 = icmp eq i64 %36, 0
  br i1 %37, label %cond1_true5, label %pair_false4, !prof !0

value_nonfalse:                                   ; preds = %continue
  %38 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %38, align 4
  ret i64 1

pair_true3:                                       ; preds = %cond1_true5
  store i64 %46, i64* %18, align 4
  %39 = load i64, i64* %20, align 4
  store i64 %39, i64* %19, align 4
  %40 = load i64, i64* %23, align 4
  store i64 %40, i64* %22, align 4
  store i64 4635845680, i64* %0, align 4
  %41 = tail call i64 inttoptr (i64 4532119872 to i64 (i64*, i64, i64*)*)(i64* nonnull %0, i64 2, i64* nonnull %19) #0
  store i64 %41, i64* %19, align 4
  %42 = icmp eq i64 %41, 66
  br i1 %42, label %undef_true7, label %continue6, !prof !1

pair_false4:                                      ; preds = %cond1_true5, %value_false
  %43 = ptrtoint i64* %18 to i64
  store i64 %43, i64* %2, align 4
  %44 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %44, align 4
  store i64 4635845728, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %35) #0
  ret i64 3

cond1_true5:                                      ; preds = %value_false
  %45 = inttoptr i64 %35 to i64*
  %46 = load i64, i64* %45, align 4
  %47 = and i64 %46, 15
  %48 = icmp eq i64 %47, 10
  br i1 %48, label %pair_false4, label %pair_true3, !prof !1

continue6:                                        ; preds = %pair_true3
  store i64 4635845664, i64* %0, align 4
  %49 = tail call i64 inttoptr (i64 4532114896 to i64 (i64*, i64, i64*)*)(i64* nonnull %0, i64 2, i64* nonnull %18) #0
  switch i64 %49, label %value_nonfalse11 [
    i64 66, label %undef_true9
    i64 34, label %value_false10
  ], !prof !2

undef_true7:                                      ; preds = %pair_true3
  %50 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %50, align 4
  ret i64 3

undef_true9:                                      ; preds = %continue6
  %51 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %51, align 4
  ret i64 3

value_false10:                                    ; preds = %continue6
  %52 = load i64, i64* %20, align 4
  store i64 %52, i64* %18, align 4
  %53 = ptrtoint i64* %19 to i64
  %54 = load i64, i64* %23, align 4
  %55 = and i64 %54, 1
  %56 = icmp eq i64 %55, 0
  br i1 %56, label %fallback, label %fixnum_true

value_nonfalse11:                                 ; preds = %continue6
  %57 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %57, align 4
  ret i64 1

continue12:                                       ; preds = %fallback, %valid_true
  %.0 = phi i64 [ %65, %valid_true ], [ %63, %fallback ]
  store i64 %.0, i64* %19, align 4
  %58 = load i64, i64* %12, align 4
  %59 = and i64 %58, 7
  %60 = icmp eq i64 %59, 0
  br i1 %60, label %cond1_true15, label %pair_false14, !prof !0

fixnum_true:                                      ; preds = %value_false10
  %61 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %54, i64 2)
  %62 = extractvalue { i64, i1 } %61, 1
  br i1 %62, label %fallback, label %valid_true, !prof !1

fallback:                                         ; preds = %fixnum_true, %value_false10
  store i64 %53, i64* %2, align 4
  %63 = tail call i64 @c_nadd_iloc(i64* nonnull %0, i64 4635846512) #0
  %64 = icmp eq i64 %63, 0
  br i1 %64, label %fallback_fail, label %continue12, !prof !1

valid_true:                                       ; preds = %fixnum_true
  %65 = extractvalue { i64, i1 } %61, 0
  br label %continue12

fallback_fail:                                    ; preds = %fallback
  ret i64 3

pair_true13:                                      ; preds = %cond1_true15
  %66 = getelementptr i64, i64* %75, i64 1
  %67 = load i64, i64* %66, align 4
  store i64 %67, i64* %22, align 4
  %68 = getelementptr i64, i64* %18, i64 3
  store i64 3, i64* %68, align 4
  %69 = getelementptr i64, i64* %18, i64 4
  store i64 0, i64* %69, align 4
  %70 = getelementptr i64, i64* %18, i64 5
  %71 = ptrtoint i64* %70 to i64
  %72 = ptrtoint i64* %69 to i64
  store i64 4635845856, i64* %0, align 4
  store i64 %71, i64* %6, align 4
  store i64 %72, i64* %3, align 4
  store i64 %71, i64* %2, align 4
  br label %tailrecurse

pair_false14:                                     ; preds = %cond1_true15, %continue12
  %73 = ptrtoint i64* %22 to i64
  store i64 %73, i64* %2, align 4
  %74 = getelementptr i64, i64* %0, i64 5
  store i64 %.0, i64* %74, align 4
  store i64 4635845584, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %58) #0
  ret i64 3

cond1_true15:                                     ; preds = %continue12
  %75 = inttoptr i64 %58 to i64*
  %76 = load i64, i64* %75, align 4
  %77 = and i64 %76, 15
  %78 = icmp eq i64 %77, 10
  br i1 %78, label %pair_false14, label %pair_true13, !prof !1
}

declare void @c_error_push_cdr_iloc(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"3f297490-673d-43f4-8aad-524975092d89"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = bitcast i64* %3 to i64**
  %5 = load i64*, i64** %4, align 4
  store i64 %2, i64* %5, align 4
  %6 = getelementptr i64, i64* %0, i64 2
  %7 = load i64, i64* %6, align 4
  store i64 4635848384, i64* %0, align 4
  %8 = tail call i64 inttoptr (i64 4532119312 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %7) #0
  %9 = icmp eq i64 %8, 66
  store i64 %8, i64* %1, align 4
  %retval = select i1 %9, i64 3, i64 1, !prof !1
  ret i64 %retval
}

attributes #0 = { nounwind }
attributes #1 = { nounwind readnone speculatable willreturn }

!0 = !{!"branch_weights", i32 100, i32 1}
!1 = !{!"branch_weights", i32 1, i32 100}
!2 = !{!"branch_weights", i32 10000, i32 101, i32 100}
