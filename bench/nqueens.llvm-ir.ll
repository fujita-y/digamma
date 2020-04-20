; ModuleID = '40a0983b-5301-4e4a-8f75-9b5f3becce22'
source_filename = "40a0983b-5301-4e4a-8f75-9b5f3becce22"

define i64 @b7745e8e-f182-4b26-8f07-785454198eea(i64* noalias nocapture %0) local_unnamed_addr {
entry:
  %1 = getelementptr i64, i64* %0, i64 6
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 7
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %2, 88
  %6 = icmp ult i64 %5, %4
  br i1 %6, label %entry.stack_ok_crit_edge, label %stack_overflow

entry.stack_ok_crit_edge:                         ; preds = %entry
  %7 = inttoptr i64 %2 to i64*
  br label %stack_ok

stack_ok:                                         ; preds = %entry.stack_ok_crit_edge, %stack_overflow
  %8 = phi i64* [ %.pre, %stack_overflow ], [ %7, %entry.stack_ok_crit_edge ]
  store i64 4526800128, i64* %8, align 4
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
  store i64 ptrtoint (i64 (i64*)* @"5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93" to i64), i64* %16, align 4
  %17 = getelementptr i64, i64* %0, i64 4
  %18 = load i64, i64* %17, align 4
  %19 = getelementptr i64, i64* %8, i64 5
  store i64 %18, i64* %19, align 4
  %20 = getelementptr i64, i64* %8, i64 6
  %21 = ptrtoint i64* %20 to i64
  %22 = ptrtoint i64* %19 to i64
  %23 = add i64 %14, -8
  %24 = inttoptr i64 %23 to i64*
  %25 = getelementptr i64, i64* %24, i64 -1
  %26 = load i64, i64* %25, align 4
  store i64 %26, i64* %20, align 4
  %27 = add i64 %21, 8
  %28 = inttoptr i64 %27 to i64*
  store i64 1, i64* %28, align 4
  %29 = getelementptr i64, i64* %28, i64 1
  store i64 0, i64* %29, align 4
  %30 = add i64 %21, 24
  %31 = ptrtoint i64* %29 to i64
  store i64 4526882496, i64* %0, align 4
  store i64 %30, i64* %10, align 4
  store i64 %31, i64* %13, align 4
  store i64 %22, i64* %17, align 4
  store i64 %30, i64* %1, align 4
  %32 = musttail call i64 @"2c696dce-7d4f-4b08-9e9c-58af994c2399"(i64* nonnull %0)
  ret i64 %32

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 88)
  %.phi.trans.insert = bitcast i64* %1 to i64**
  %.pre = load i64*, i64** %.phi.trans.insert, align 4
  br label %stack_ok
}

declare void @c_collect_stack(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = inttoptr i64 %4 to i64*
  store i64 %2, i64* %5, align 4
  %6 = add i64 %4, 8
  %7 = inttoptr i64 %6 to i64*
  store i64 50, i64* %7, align 4
  %8 = add i64 %4, 16
  %9 = inttoptr i64 %8 to i64*
  store i64 50, i64* %9, align 4
  %10 = add i64 %4, 24
  %11 = inttoptr i64 %10 to i64*
  store i64 3, i64* %11, align 4
  %12 = getelementptr i64, i64* %11, i64 1
  store i64 0, i64* %12, align 4
  %13 = add i64 %4, 40
  %14 = ptrtoint i64* %12 to i64
  store i64 4526877904, i64* %0, align 4
  %15 = getelementptr i64, i64* %0, i64 2
  store i64 %13, i64* %15, align 4
  %16 = getelementptr i64, i64* %0, i64 3
  store i64 %14, i64* %16, align 4
  store i64 %13, i64* %3, align 4
  %17 = musttail call i64 @"051cca58-8c2c-49b9-9a86-c080a588ede6"(i64* nonnull %0)
  ret i64 %17
}

; Function Attrs: nounwind
define i64 @"2c696dce-7d4f-4b08-9e9c-58af994c2399"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 6
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 7
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %2, 32
  %6 = icmp ult i64 %5, %4
  br i1 %6, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %entry
  %.pre-phi = phi i64 [ %.pre1, %stack_overflow ], [ %5, %entry ]
  %7 = phi i64 [ %.pre, %stack_overflow ], [ %2, %entry ]
  %8 = getelementptr i64, i64* %0, i64 3
  %9 = load i64, i64* %8, align 4
  %10 = add i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  %12 = getelementptr i64, i64* %11, i64 -1
  %13 = load i64, i64* %12, align 4
  %14 = inttoptr i64 %7 to i64*
  store i64 %13, i64* %14, align 4
  %15 = add i64 %7, 8
  %16 = inttoptr i64 %15 to i64*
  store i64 50, i64* %16, align 4
  %17 = add i64 %7, 16
  %18 = inttoptr i64 %17 to i64*
  store i64 2, i64* %18, align 4
  %19 = getelementptr i64, i64* %18, i64 1
  store i64 0, i64* %19, align 4
  %20 = ptrtoint i64* %19 to i64
  store i64 4526881376, i64* %0, align 4
  %21 = getelementptr i64, i64* %0, i64 2
  store i64 %.pre-phi, i64* %21, align 4
  store i64 %20, i64* %8, align 4
  store i64 %.pre-phi, i64* %1, align 4
  %22 = musttail call i64 @a86247ed-3932-42fe-8b12-f6f7e6ad581c(i64* nonnull %0)
  ret i64 %22

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  %.pre = load i64, i64* %1, align 4
  %.pre1 = add i64 %.pre, 32
  br label %stack_ok
}

; Function Attrs: nounwind
define i64 @a86247ed-3932-42fe-8b12-f6f7e6ad581c(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 6
  %2 = getelementptr i64, i64* %0, i64 7
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = getelementptr i64, i64* %0, i64 5
  %5 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %1, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %continue1, %entry
  %6 = phi i64 [ %37, %continue1 ], [ %.pre, %entry ]
  %7 = load i64, i64* %2, align 4
  %8 = add i64 %6, 32
  %9 = icmp ult i64 %8, %7
  br i1 %9, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %10 = load i64, i64* %3, align 4
  %11 = add i64 %10, -8
  %12 = inttoptr i64 %11 to i64*
  %13 = getelementptr i64, i64* %12, i64 -2
  %14 = load i64, i64* %13, align 4
  %15 = and i64 %14, 1
  %16 = icmp eq i64 %15, 0
  br i1 %16, label %nonfixnum_true, label %nonfixnum_false

stack_overflow:                                   ; preds = %tailrecurse
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  br label %stack_ok

continue:                                         ; preds = %nonfixnum_true
  %.pre6 = load i64, i64* %4, align 4
  %17 = icmp eq i64 %.pre6, 34
  br i1 %17, label %f9h_true, label %f9h_false

nonfixnum_true:                                   ; preds = %stack_ok
  store i64 4526881376, i64* %0, align 4
  %18 = tail call i64 @c_eq_n_iloc(i64* nonnull %0, i64 %14, i64 1) #0
  %19 = icmp eq i64 %18, 0
  br i1 %19, label %continue, label %fallback_fail

nonfixnum_false:                                  ; preds = %stack_ok
  %20 = icmp eq i64 %14, 1
  br i1 %20, label %continue.thread8, label %continue.thread

continue.thread8:                                 ; preds = %nonfixnum_false
  store i64 18, i64* %4, align 4
  br label %f9h_false

continue.thread:                                  ; preds = %nonfixnum_false
  store i64 34, i64* %4, align 4
  br label %f9h_true

fallback_fail:                                    ; preds = %fallback, %nonfixnum_true
  ret i64 3

f9h_true:                                         ; preds = %continue.thread, %continue
  %21 = load i64, i64* %13, align 4
  %22 = and i64 %21, 1
  %23 = icmp eq i64 %22, 0
  br i1 %23, label %fallback, label %fixnum_true

f9h_false:                                        ; preds = %continue, %continue.thread8
  %24 = getelementptr i64, i64* %12, i64 -1
  %25 = load i64, i64* %24, align 4
  store i64 %25, i64* %4, align 4
  ret i64 1

continue1:                                        ; preds = %fallback.continue1_crit_edge, %valid_true
  %26 = phi i64 [ %.pre7, %fallback.continue1_crit_edge ], [ %46, %valid_true ]
  %27 = load i64, i64* %13, align 4
  %28 = inttoptr i64 %26 to i64*
  store i64 %27, i64* %28, align 4
  %29 = add i64 %26, 8
  %30 = getelementptr i64, i64* %12, i64 -1
  %31 = load i64, i64* %30, align 4
  %32 = inttoptr i64 %29 to i64*
  %33 = getelementptr i64, i64* %32, i64 -1
  %34 = load i64, i64* %33, align 4
  %35 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %34, i64 %31) #0
  store i64 %35, i64* %33, align 4
  store i64 2, i64* %32, align 4
  %36 = getelementptr i64, i64* %32, i64 1
  store i64 0, i64* %36, align 4
  %37 = add i64 %26, 24
  %38 = ptrtoint i64* %36 to i64
  store i64 4526881376, i64* %0, align 4
  store i64 %37, i64* %5, align 4
  store i64 %38, i64* %3, align 4
  store i64 %37, i64* %1, align 4
  br label %tailrecurse

fixnum_true:                                      ; preds = %f9h_true
  %39 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %21, i64 -2)
  %40 = extractvalue { i64, i1 } %39, 1
  br i1 %40, label %fallback, label %valid_true

fallback:                                         ; preds = %fixnum_true, %f9h_true
  %41 = tail call i64 @c_push_nadd_iloc(i64* nonnull %0, i64 4526881968) #0
  %42 = icmp eq i64 %41, 0
  br i1 %42, label %fallback.continue1_crit_edge, label %fallback_fail

fallback.continue1_crit_edge:                     ; preds = %fallback
  %.pre7 = load i64, i64* %1, align 4
  br label %continue1

valid_true:                                       ; preds = %fixnum_true
  %43 = extractvalue { i64, i1 } %39, 0
  %44 = load i64, i64* %1, align 4
  %45 = inttoptr i64 %44 to i64*
  store i64 %43, i64* %45, align 4
  %46 = add i64 %44, 8
  store i64 %46, i64* %1, align 4
  br label %continue1
}

declare i64 @c_eq_n_iloc(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i64 @c_push_nadd_iloc(i64*, i64) local_unnamed_addr

declare i64 @c_make_pair(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"051cca58-8c2c-49b9-9a86-c080a588ede6"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 6
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 7
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %2, 152
  %6 = icmp ult i64 %5, %4
  br i1 %6, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %entry
  %7 = getelementptr i64, i64* %0, i64 3
  %8 = load i64, i64* %7, align 4
  %9 = add i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  %11 = getelementptr i64, i64* %10, i64 -3
  %12 = load i64, i64* %11, align 4
  %13 = icmp eq i64 %12, 50
  br i1 %13, label %taken_true, label %taken_false

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 152) #0
  br label %stack_ok

taken_true:                                       ; preds = %stack_ok
  %14 = getelementptr i64, i64* %10, i64 -2
  %15 = load i64, i64* %14, align 4
  %16 = icmp eq i64 %15, 50
  %17 = getelementptr i64, i64* %0, i64 5
  br i1 %16, label %taken_true1, label %taken_false2

taken_false:                                      ; preds = %stack_ok
  %18 = bitcast i64* %1 to i64**
  %19 = load i64*, i64** %18, align 4
  store i64 4526877856, i64* %19, align 4
  %20 = getelementptr i64, i64* %19, i64 1
  store i64 82, i64* %20, align 4
  %21 = getelementptr i64, i64* %0, i64 2
  %22 = load i64, i64* %21, align 4
  %23 = getelementptr i64, i64* %19, i64 2
  store i64 %22, i64* %23, align 4
  %24 = getelementptr i64, i64* %19, i64 3
  store i64 %8, i64* %24, align 4
  %25 = getelementptr i64, i64* %19, i64 4
  store i64 ptrtoint (i64 (i64*)* @d070c580-04b2-414c-9f5a-37ef01b58089 to i64), i64* %25, align 4
  %26 = getelementptr i64, i64* %0, i64 4
  %27 = load i64, i64* %26, align 4
  %28 = getelementptr i64, i64* %19, i64 5
  store i64 %27, i64* %28, align 4
  %29 = getelementptr i64, i64* %19, i64 6
  %30 = ptrtoint i64* %29 to i64
  %31 = ptrtoint i64* %28 to i64
  store i64 4526879136, i64* %29, align 4
  %32 = getelementptr i64, i64* %19, i64 7
  store i64 82, i64* %32, align 4
  %33 = getelementptr i64, i64* %19, i64 8
  store i64 %30, i64* %33, align 4
  %34 = getelementptr i64, i64* %19, i64 9
  store i64 %8, i64* %34, align 4
  %35 = getelementptr i64, i64* %19, i64 10
  store i64 ptrtoint (i64 (i64*)* @"44d6386f-e8e4-4381-9c0f-5ad845cc8da7" to i64), i64* %35, align 4
  %36 = getelementptr i64, i64* %19, i64 11
  store i64 %31, i64* %36, align 4
  %37 = getelementptr i64, i64* %19, i64 12
  %38 = ptrtoint i64* %37 to i64
  %39 = ptrtoint i64* %36 to i64
  %40 = load i64, i64* %11, align 4
  %41 = and i64 %40, 7
  %42 = icmp eq i64 %41, 0
  br i1 %42, label %cond1_true, label %pair_false

taken_true1:                                      ; preds = %taken_true
  store i64 3, i64* %17, align 4
  ret i64 1

taken_false2:                                     ; preds = %taken_true
  store i64 1, i64* %17, align 4
  ret i64 1

pair_true:                                        ; preds = %cond1_true
  store i64 %57, i64* %37, align 4
  %43 = add i64 %38, 8
  %44 = inttoptr i64 %43 to i64*
  store i64 3, i64* %44, align 4
  %45 = add i64 %38, 16
  %46 = getelementptr i64, i64* %10, i64 -1
  %47 = load i64, i64* %46, align 4
  %48 = inttoptr i64 %45 to i64*
  store i64 %47, i64* %48, align 4
  %49 = add i64 %38, 24
  %50 = inttoptr i64 %49 to i64*
  store i64 3, i64* %50, align 4
  %51 = getelementptr i64, i64* %50, i64 1
  store i64 0, i64* %51, align 4
  %52 = add i64 %38, 40
  %53 = ptrtoint i64* %51 to i64
  store i64 4526875280, i64* %0, align 4
  store i64 %52, i64* %21, align 4
  store i64 %53, i64* %7, align 4
  store i64 %39, i64* %26, align 4
  store i64 %52, i64* %1, align 4
  %54 = musttail call i64 @b2c68e4c-59b7-4e71-9792-323c589decc3(i64* nonnull %0)
  ret i64 %54

pair_false:                                       ; preds = %cond1_true, %taken_false
  store i64 %38, i64* %21, align 4
  store i64 %39, i64* %26, align 4
  store i64 %38, i64* %1, align 4
  %55 = getelementptr i64, i64* %0, i64 5
  store i64 %12, i64* %55, align 4
  store i64 4526880320, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %40) #0
  ret i64 3

cond1_true:                                       ; preds = %taken_false
  %56 = inttoptr i64 %40 to i64*
  %57 = load i64, i64* %56, align 4
  %58 = and i64 %57, 15
  %59 = icmp eq i64 %58, 10
  br i1 %59, label %pair_false, label %pair_true
}

; Function Attrs: nounwind
define i64 @d070c580-04b2-414c-9f5a-37ef01b58089(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = inttoptr i64 %4 to i64*
  store i64 %2, i64* %5, align 4
  %6 = add i64 %4, 8
  %7 = inttoptr i64 %6 to i64*
  store i64 4526877824, i64* %7, align 4
  %8 = getelementptr i64, i64* %7, i64 1
  store i64 82, i64* %8, align 4
  %9 = getelementptr i64, i64* %0, i64 2
  %10 = load i64, i64* %9, align 4
  %11 = getelementptr i64, i64* %7, i64 2
  store i64 %10, i64* %11, align 4
  %12 = getelementptr i64, i64* %0, i64 3
  %13 = load i64, i64* %12, align 4
  %14 = getelementptr i64, i64* %7, i64 3
  store i64 %13, i64* %14, align 4
  %15 = getelementptr i64, i64* %7, i64 4
  store i64 ptrtoint (i64 (i64*)* @"2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2" to i64), i64* %15, align 4
  %16 = getelementptr i64, i64* %0, i64 4
  %17 = load i64, i64* %16, align 4
  %18 = getelementptr i64, i64* %7, i64 5
  store i64 %17, i64* %18, align 4
  %19 = getelementptr i64, i64* %7, i64 6
  %20 = ptrtoint i64* %19 to i64
  %21 = ptrtoint i64* %18 to i64
  %22 = add i64 %13, -8
  %23 = inttoptr i64 %22 to i64*
  %24 = getelementptr i64, i64* %23, i64 -3
  %25 = load i64, i64* %24, align 4
  %26 = and i64 %25, 7
  %27 = icmp eq i64 %26, 0
  br i1 %27, label %cond1_true, label %pair_false

pair_true:                                        ; preds = %cond1_true
  %28 = getelementptr i64, i64* %34, i64 1
  %29 = load i64, i64* %28, align 4
  store i64 %29, i64* %19, align 4
  %30 = add i64 %20, 8
  %31 = load i64, i64* %24, align 4
  %32 = and i64 %31, 7
  %33 = icmp eq i64 %32, 0
  br i1 %33, label %cond1_true3, label %pair_false2

pair_false:                                       ; preds = %cond1_true, %entry
  store i64 %20, i64* %9, align 4
  store i64 %21, i64* %16, align 4
  store i64 %20, i64* %3, align 4
  store i64 4526878336, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %25) #0
  ret i64 3

cond1_true:                                       ; preds = %entry
  %34 = inttoptr i64 %25 to i64*
  %35 = load i64, i64* %34, align 4
  %36 = and i64 %35, 15
  %37 = icmp eq i64 %36, 10
  br i1 %37, label %pair_false, label %pair_true

pair_true1:                                       ; preds = %cond1_true3
  %38 = inttoptr i64 %30 to i64*
  store i64 %55, i64* %38, align 4
  %39 = add i64 %20, 16
  %40 = getelementptr i64, i64* %23, i64 -2
  %41 = load i64, i64* %40, align 4
  %42 = inttoptr i64 %39 to i64*
  %43 = getelementptr i64, i64* %42, i64 -1
  %44 = load i64, i64* %43, align 4
  %45 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %44, i64 %41) #0
  store i64 %45, i64* %43, align 4
  %46 = getelementptr i64, i64* %23, i64 -1
  %47 = load i64, i64* %46, align 4
  store i64 %47, i64* %42, align 4
  %48 = add i64 %20, 24
  %49 = inttoptr i64 %48 to i64*
  store i64 3, i64* %49, align 4
  %50 = getelementptr i64, i64* %49, i64 1
  store i64 0, i64* %50, align 4
  %51 = add i64 %20, 40
  %52 = ptrtoint i64* %50 to i64
  store i64 4526877904, i64* %0, align 4
  store i64 %51, i64* %9, align 4
  store i64 %52, i64* %12, align 4
  store i64 %21, i64* %16, align 4
  store i64 %51, i64* %3, align 4
  %53 = musttail call i64 @"051cca58-8c2c-49b9-9a86-c080a588ede6"(i64* nonnull %0)
  ret i64 %53

pair_false2:                                      ; preds = %cond1_true3, %pair_true
  store i64 %20, i64* %9, align 4
  store i64 %21, i64* %16, align 4
  store i64 %30, i64* %3, align 4
  store i64 4526878320, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %31) #0
  ret i64 3

cond1_true3:                                      ; preds = %pair_true
  %54 = inttoptr i64 %31 to i64*
  %55 = load i64, i64* %54, align 4
  %56 = and i64 %55, 15
  %57 = icmp eq i64 %56, 10
  br i1 %57, label %pair_false2, label %pair_true1
}

; Function Attrs: nounwind
define i64 @"44d6386f-e8e4-4381-9c0f-5ad845cc8da7"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = icmp eq i64 %2, 34
  br i1 %3, label %f9h_true, label %f9h_false

f9h_true:                                         ; preds = %entry
  store i64 1, i64* %1, align 4
  ret i64 1

f9h_false:                                        ; preds = %entry
  %4 = getelementptr i64, i64* %0, i64 3
  %5 = load i64, i64* %4, align 4
  %6 = add i64 %5, -8
  %7 = inttoptr i64 %6 to i64*
  %8 = getelementptr i64, i64* %7, i64 -3
  %9 = load i64, i64* %8, align 4
  %10 = and i64 %9, 7
  %11 = icmp eq i64 %10, 0
  br i1 %11, label %cond1_true, label %pair_false

pair_true:                                        ; preds = %cond1_true
  %12 = getelementptr i64, i64* %24, i64 1
  %13 = load i64, i64* %12, align 4
  %14 = getelementptr i64, i64* %0, i64 6
  %15 = load i64, i64* %14, align 4
  %16 = inttoptr i64 %15 to i64*
  store i64 %13, i64* %16, align 4
  %17 = add i64 %15, 8
  %18 = getelementptr i64, i64* %7, i64 -2
  %19 = load i64, i64* %18, align 4
  %20 = inttoptr i64 %17 to i64*
  store i64 %19, i64* %20, align 4
  %21 = add i64 %15, 16
  store i64 4526879248, i64* %0, align 4
  %22 = tail call i64 inttoptr (i64 4404351056 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %15) #0
  store i64 %22, i64* %16, align 4
  %23 = icmp eq i64 %22, 66
  br i1 %23, label %undef_true, label %continue

pair_false:                                       ; preds = %cond1_true, %f9h_false
  store i64 4526879280, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %9) #0
  ret i64 3

cond1_true:                                       ; preds = %f9h_false
  %24 = inttoptr i64 %9 to i64*
  %25 = load i64, i64* %24, align 4
  %26 = and i64 %25, 15
  %27 = icmp eq i64 %26, 10
  br i1 %27, label %pair_false, label %pair_true

continue:                                         ; preds = %pair_true
  store i64 50, i64* %20, align 4
  %28 = load i64, i64* %8, align 4
  %29 = and i64 %28, 7
  %30 = icmp eq i64 %29, 0
  br i1 %30, label %cond1_true3, label %pair_false2

undef_true:                                       ; preds = %pair_true
  store i64 66, i64* %1, align 4
  ret i64 3

pair_true1:                                       ; preds = %cond1_true3
  %31 = inttoptr i64 %21 to i64*
  store i64 %45, i64* %31, align 4
  %32 = add i64 %15, 24
  %33 = getelementptr i64, i64* %7, i64 -1
  %34 = load i64, i64* %33, align 4
  %35 = inttoptr i64 %32 to i64*
  %36 = getelementptr i64, i64* %35, i64 -1
  %37 = load i64, i64* %36, align 4
  %38 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %37, i64 %34) #0
  store i64 %38, i64* %36, align 4
  store i64 3, i64* %35, align 4
  %39 = getelementptr i64, i64* %35, i64 1
  store i64 0, i64* %39, align 4
  %40 = add i64 %15, 40
  %41 = ptrtoint i64* %39 to i64
  store i64 4526877904, i64* %0, align 4
  %42 = getelementptr i64, i64* %0, i64 2
  store i64 %40, i64* %42, align 4
  store i64 %41, i64* %4, align 4
  store i64 %40, i64* %14, align 4
  %43 = musttail call i64 @"051cca58-8c2c-49b9-9a86-c080a588ede6"(i64* nonnull %0)
  ret i64 %43

pair_false2:                                      ; preds = %cond1_true3, %continue
  store i64 %21, i64* %14, align 4
  store i64 %22, i64* %1, align 4
  store i64 4526879216, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %28) #0
  ret i64 3

cond1_true3:                                      ; preds = %continue
  %44 = inttoptr i64 %28 to i64*
  %45 = load i64, i64* %44, align 4
  %46 = and i64 %45, 15
  %47 = icmp eq i64 %46, 10
  br i1 %47, label %pair_false2, label %pair_true1
}

declare void @c_error_push_car_iloc(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @b2c68e4c-59b7-4e71-9792-323c589decc3(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 6
  %2 = getelementptr i64, i64* %0, i64 7
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = getelementptr i64, i64* %0, i64 5
  %5 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %1, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %pair_true13, %entry
  %6 = phi i64 [ %66, %pair_true13 ], [ %.pre, %entry ]
  %7 = load i64, i64* %2, align 4
  %8 = add i64 %6, 104
  %9 = icmp ult i64 %8, %7
  br i1 %9, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %10 = load i64, i64* %3, align 4
  %11 = add i64 %10, -8
  %12 = inttoptr i64 %11 to i64*
  %13 = getelementptr i64, i64* %12, i64 -1
  %14 = load i64, i64* %13, align 4
  %15 = icmp eq i64 %14, 50
  br i1 %15, label %taken_true, label %taken_false

stack_overflow:                                   ; preds = %tailrecurse
  tail call void @c_collect_stack(i64* nonnull %0, i64 104) #0
  br label %stack_ok

taken_true:                                       ; preds = %stack_ok
  store i64 18, i64* %4, align 4
  ret i64 1

taken_false:                                      ; preds = %stack_ok
  %16 = and i64 %14, 7
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %cond1_true, label %pair_false

pair_true:                                        ; preds = %cond1_true
  %18 = load i64, i64* %1, align 4
  %19 = inttoptr i64 %18 to i64*
  store i64 %31, i64* %19, align 4
  %20 = add i64 %18, 8
  %21 = getelementptr i64, i64* %12, i64 -3
  %22 = load i64, i64* %21, align 4
  %23 = inttoptr i64 %20 to i64*
  store i64 %22, i64* %23, align 4
  %24 = add i64 %18, 16
  %25 = getelementptr i64, i64* %12, i64 -2
  %26 = load i64, i64* %25, align 4
  %27 = inttoptr i64 %24 to i64*
  store i64 %26, i64* %27, align 4
  store i64 4526875200, i64* %0, align 4
  %28 = tail call i64 inttoptr (i64 4404393696 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %20) #0
  store i64 %28, i64* %23, align 4
  %29 = icmp eq i64 %28, 66
  br i1 %29, label %undef_true, label %continue

pair_false:                                       ; preds = %cond1_true, %taken_false
  store i64 18, i64* %4, align 4
  store i64 4526875248, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %14) #0
  ret i64 3

cond1_true:                                       ; preds = %taken_false
  %30 = inttoptr i64 %14 to i64*
  %31 = load i64, i64* %30, align 4
  %32 = and i64 %31, 15
  %33 = icmp eq i64 %32, 10
  br i1 %33, label %pair_false, label %pair_true

continue:                                         ; preds = %pair_true
  store i64 4526875184, i64* %0, align 4
  %34 = tail call i64 inttoptr (i64 4404389280 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %18) #0
  switch i64 %34, label %value_nonfalse [
    i64 66, label %undef_true2
    i64 34, label %value_false
  ]

undef_true:                                       ; preds = %pair_true
  store i64 66, i64* %4, align 4
  ret i64 3

undef_true2:                                      ; preds = %continue
  store i64 66, i64* %4, align 4
  ret i64 3

value_false:                                      ; preds = %continue
  %35 = load i64, i64* %13, align 4
  %36 = and i64 %35, 7
  %37 = icmp eq i64 %36, 0
  br i1 %37, label %cond1_true5, label %pair_false4

value_nonfalse:                                   ; preds = %continue
  store i64 34, i64* %4, align 4
  ret i64 1

pair_true3:                                       ; preds = %cond1_true5
  store i64 %43, i64* %19, align 4
  %38 = load i64, i64* %21, align 4
  store i64 %38, i64* %23, align 4
  %39 = load i64, i64* %25, align 4
  store i64 %39, i64* %27, align 4
  store i64 4526875104, i64* %0, align 4
  %40 = tail call i64 inttoptr (i64 4404394256 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %20) #0
  store i64 %40, i64* %23, align 4
  %41 = icmp eq i64 %40, 66
  br i1 %41, label %undef_true7, label %continue6

pair_false4:                                      ; preds = %cond1_true5, %value_false
  store i64 %18, i64* %1, align 4
  store i64 34, i64* %4, align 4
  store i64 4526875152, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %35) #0
  ret i64 3

cond1_true5:                                      ; preds = %value_false
  %42 = inttoptr i64 %35 to i64*
  %43 = load i64, i64* %42, align 4
  %44 = and i64 %43, 15
  %45 = icmp eq i64 %44, 10
  br i1 %45, label %pair_false4, label %pair_true3

continue6:                                        ; preds = %pair_true3
  store i64 4526875088, i64* %0, align 4
  %46 = tail call i64 inttoptr (i64 4404389280 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %18) #0
  switch i64 %46, label %value_nonfalse11 [
    i64 66, label %undef_true9
    i64 34, label %value_false10
  ]

undef_true7:                                      ; preds = %pair_true3
  store i64 66, i64* %4, align 4
  ret i64 3

undef_true9:                                      ; preds = %continue6
  store i64 66, i64* %4, align 4
  ret i64 3

value_false10:                                    ; preds = %continue6
  %47 = load i64, i64* %21, align 4
  store i64 %47, i64* %19, align 4
  %48 = load i64, i64* %25, align 4
  %49 = and i64 %48, 1
  %50 = icmp eq i64 %49, 0
  br i1 %50, label %fallback, label %fixnum_true

value_nonfalse11:                                 ; preds = %continue6
  store i64 34, i64* %4, align 4
  ret i64 1

continue12:                                       ; preds = %fallback, %valid_true
  %51 = load i64, i64* %13, align 4
  %52 = and i64 %51, 7
  %53 = icmp eq i64 %52, 0
  br i1 %53, label %cond1_true15, label %pair_false14

fixnum_true:                                      ; preds = %value_false10
  %54 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %48, i64 2)
  %55 = extractvalue { i64, i1 } %54, 1
  br i1 %55, label %fallback, label %valid_true

fallback:                                         ; preds = %fixnum_true, %value_false10
  store i64 %20, i64* %1, align 4
  store i64 34, i64* %4, align 4
  %56 = tail call i64 @c_push_nadd_iloc(i64* nonnull %0, i64 4526875936) #0
  %57 = icmp eq i64 %56, 0
  br i1 %57, label %continue12, label %fallback_fail

valid_true:                                       ; preds = %fixnum_true
  %58 = extractvalue { i64, i1 } %54, 0
  store i64 %58, i64* %23, align 4
  store i64 %24, i64* %1, align 4
  br label %continue12

fallback_fail:                                    ; preds = %fallback
  ret i64 3

pair_true13:                                      ; preds = %cond1_true15
  %59 = getelementptr i64, i64* %68, i64 1
  %60 = load i64, i64* %59, align 4
  %61 = load i64, i64* %1, align 4
  %62 = inttoptr i64 %61 to i64*
  store i64 %60, i64* %62, align 4
  %63 = add i64 %61, 8
  %64 = inttoptr i64 %63 to i64*
  store i64 3, i64* %64, align 4
  %65 = getelementptr i64, i64* %64, i64 1
  store i64 0, i64* %65, align 4
  %66 = add i64 %61, 24
  %67 = ptrtoint i64* %65 to i64
  store i64 4526875280, i64* %0, align 4
  store i64 %66, i64* %5, align 4
  store i64 %67, i64* %3, align 4
  store i64 %66, i64* %1, align 4
  br label %tailrecurse

pair_false14:                                     ; preds = %cond1_true15, %continue12
  store i64 34, i64* %4, align 4
  store i64 4526875008, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %51) #0
  ret i64 3

cond1_true15:                                     ; preds = %continue12
  %68 = inttoptr i64 %51 to i64*
  %69 = load i64, i64* %68, align 4
  %70 = and i64 %69, 15
  %71 = icmp eq i64 %70, 10
  br i1 %71, label %pair_false14, label %pair_true13
}

declare void @c_error_push_cdr_iloc(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @"2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2"(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = bitcast i64* %3 to i64**
  %5 = load i64*, i64** %4, align 4
  store i64 %2, i64* %5, align 4
  %6 = getelementptr i64, i64* %0, i64 2
  %7 = load i64, i64* %6, align 4
  store i64 4526877808, i64* %0, align 4
  %8 = tail call i64 inttoptr (i64 4404393696 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %7) #0
  %9 = icmp eq i64 %8, 66
  store i64 %8, i64* %1, align 4
  %retval = select i1 %9, i64 3, i64 1
  ret i64 %retval
}

attributes #0 = { nounwind }
attributes #1 = { nounwind readnone speculatable willreturn }
