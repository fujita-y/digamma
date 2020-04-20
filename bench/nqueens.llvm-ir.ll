;*** IR after optimize ***
; ModuleID = '9696a26c-1948-4aec-9eac-1f775393f37c'
source_filename = "9696a26c-1948-4aec-9eac-1f775393f37c"

define i64 @"48606894-c232-44a1-835f-2a253e06472e"(i64* noalias nocapture %0) local_unnamed_addr {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 88
  %6 = icmp ult i64 %5, %2
  br i1 %6, label %entry.stack_ok_crit_edge, label %stack_overflow

entry.stack_ok_crit_edge:                         ; preds = %entry
  %7 = inttoptr i64 %4 to i64*
  br label %stack_ok

stack_ok:                                         ; preds = %entry.stack_ok_crit_edge, %stack_overflow
  %8 = phi i64* [ %.pre, %stack_overflow ], [ %7, %entry.stack_ok_crit_edge ]
  store i64 4401937424, i64* %8, align 4
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
  store i64 ptrtoint (i64 (i64*)* @bf8534cd-c97d-489d-8941-1e597392dba3 to i64), i64* %16, align 4
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
  store i64 4402016976, i64* %0, align 4
  store i64 %30, i64* %10, align 4
  store i64 %31, i64* %13, align 4
  store i64 %22, i64* %17, align 4
  store i64 %30, i64* %3, align 4
  %32 = musttail call i64 @"47d922f7-5ea0-486b-94da-c9dc0977b2c1"(i64* nonnull %0)
  ret i64 %32

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 88)
  %.phi.trans.insert = bitcast i64* %3 to i64**
  %.pre = load i64*, i64** %.phi.trans.insert, align 4
  br label %stack_ok
}

declare void @c_collect_stack(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @bf8534cd-c97d-489d-8941-1e597392dba3(i64* noalias nocapture %0) #0 {
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
  store i64 4402013568, i64* %0, align 4
  %15 = getelementptr i64, i64* %0, i64 2
  store i64 %13, i64* %15, align 4
  %16 = getelementptr i64, i64* %0, i64 3
  store i64 %14, i64* %16, align 4
  store i64 %13, i64* %3, align 4
  %17 = musttail call i64 @e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa(i64* nonnull %0)
  ret i64 %17
}

; Function Attrs: nounwind
define i64 @"47d922f7-5ea0-486b-94da-c9dc0977b2c1"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 32
  %6 = icmp ult i64 %5, %2
  br i1 %6, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %entry
  %.pre-phi = phi i64 [ %.pre1, %stack_overflow ], [ %5, %entry ]
  %7 = phi i64 [ %.pre, %stack_overflow ], [ %4, %entry ]
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
  store i64 4402012400, i64* %0, align 4
  %21 = getelementptr i64, i64* %0, i64 2
  store i64 %.pre-phi, i64* %21, align 4
  store i64 %20, i64* %8, align 4
  store i64 %.pre-phi, i64* %3, align 4
  %22 = musttail call i64 @"9a10efff-cc4c-4d72-99c6-4efd419644d9"(i64* nonnull %0)
  ret i64 %22

stack_overflow:                                   ; preds = %entry
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  %.pre = load i64, i64* %3, align 4
  %.pre1 = add i64 %.pre, 32
  br label %stack_ok
}

; Function Attrs: nounwind
define i64 @"9a10efff-cc4c-4d72-99c6-4efd419644d9"(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = getelementptr i64, i64* %0, i64 6
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %2, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %continue1, %entry
  %5 = phi i64 [ %35, %continue1 ], [ %.pre, %entry ]
  %6 = load i64, i64* %1, align 4
  %7 = add i64 %5, 32
  %8 = icmp ult i64 %7, %6
  br i1 %8, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %9 = load i64, i64* %3, align 4
  %10 = add i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  %12 = getelementptr i64, i64* %11, i64 -2
  %13 = load i64, i64* %12, align 4
  %14 = and i64 %13, 1
  %15 = icmp eq i64 %14, 0
  br i1 %15, label %nonfixnum_true, label %nonfixnum_false

stack_overflow:                                   ; preds = %tailrecurse
  tail call void @c_collect_stack(i64* nonnull %0, i64 32) #0
  br label %stack_ok

nonfixnum_true:                                   ; preds = %stack_ok
  store i64 4402012400, i64* %0, align 4
  %16 = tail call i64 @c_eq_n_iloc(i64* nonnull %0, i64 %13, i64 1) #0
  switch i64 %16, label %f9h_false [
    i64 0, label %fallback_fail
    i64 34, label %f9h_true
  ]

nonfixnum_false:                                  ; preds = %stack_ok
  %17 = icmp eq i64 %13, 1
  br i1 %17, label %f9h_false, label %fixnum_true

fallback_fail:                                    ; preds = %nonfixnum_true, %fallback
  ret i64 3

f9h_true:                                         ; preds = %nonfixnum_true
  %.pre10 = load i64, i64* %12, align 4
  %.pre11 = and i64 %.pre10, 1
  %18 = icmp eq i64 %.pre11, 0
  br i1 %18, label %fallback, label %fixnum_true

f9h_false:                                        ; preds = %nonfixnum_false, %nonfixnum_true
  %19 = getelementptr i64, i64* %11, i64 -1
  %20 = load i64, i64* %19, align 4
  %21 = getelementptr i64, i64* %0, i64 5
  store i64 %20, i64* %21, align 4
  ret i64 1

continue1:                                        ; preds = %fallback, %valid_true
  %.05 = phi i64 [ %42, %valid_true ], [ %40, %fallback ]
  %22 = load i64, i64* %2, align 4
  %23 = inttoptr i64 %22 to i64*
  store i64 %.05, i64* %23, align 4
  %24 = add i64 %22, 8
  %25 = load i64, i64* %12, align 4
  %26 = inttoptr i64 %24 to i64*
  store i64 %25, i64* %26, align 4
  %27 = add i64 %22, 16
  %28 = getelementptr i64, i64* %11, i64 -1
  %29 = load i64, i64* %28, align 4
  %30 = inttoptr i64 %27 to i64*
  %31 = getelementptr i64, i64* %30, i64 -1
  %32 = load i64, i64* %31, align 4
  %33 = tail call i64 @c_make_pair(i64* nonnull %0, i64 %32, i64 %29) #0
  store i64 %33, i64* %31, align 4
  store i64 2, i64* %30, align 4
  %34 = getelementptr i64, i64* %30, i64 1
  store i64 0, i64* %34, align 4
  %35 = add i64 %22, 32
  %36 = ptrtoint i64* %34 to i64
  store i64 4402012400, i64* %0, align 4
  store i64 %35, i64* %4, align 4
  store i64 %36, i64* %3, align 4
  store i64 %35, i64* %2, align 4
  br label %tailrecurse

fixnum_true:                                      ; preds = %nonfixnum_false, %f9h_true
  %37 = phi i64 [ %.pre10, %f9h_true ], [ %13, %nonfixnum_false ]
  %38 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %37, i64 -2)
  %39 = extractvalue { i64, i1 } %38, 1
  br i1 %39, label %fallback, label %valid_true

fallback:                                         ; preds = %fixnum_true, %f9h_true
  %40 = tail call i64 @c_nadd_iloc(i64* nonnull %0, i64 4402012992) #0
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %fallback_fail, label %continue1

valid_true:                                       ; preds = %fixnum_true
  %42 = extractvalue { i64, i1 } %38, 0
  br label %continue1
}

declare i64 @c_eq_n_iloc(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64) #1

declare i64 @c_nadd_iloc(i64*, i64) local_unnamed_addr

declare i64 @c_make_pair(i64*, i64, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = add i64 %4, 152
  %6 = icmp ult i64 %5, %2
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
  %18 = bitcast i64* %3 to i64**
  %19 = load i64*, i64** %18, align 4
  store i64 4402013520, i64* %19, align 4
  %20 = getelementptr i64, i64* %19, i64 1
  store i64 82, i64* %20, align 4
  %21 = getelementptr i64, i64* %0, i64 2
  %22 = load i64, i64* %21, align 4
  %23 = getelementptr i64, i64* %19, i64 2
  store i64 %22, i64* %23, align 4
  %24 = getelementptr i64, i64* %19, i64 3
  store i64 %8, i64* %24, align 4
  %25 = getelementptr i64, i64* %19, i64 4
  store i64 ptrtoint (i64 (i64*)* @f58699b3-5194-4b6f-93e7-6bb290659cd4 to i64), i64* %25, align 4
  %26 = getelementptr i64, i64* %0, i64 4
  %27 = load i64, i64* %26, align 4
  %28 = getelementptr i64, i64* %19, i64 5
  store i64 %27, i64* %28, align 4
  %29 = getelementptr i64, i64* %19, i64 6
  %30 = ptrtoint i64* %29 to i64
  %31 = ptrtoint i64* %28 to i64
  store i64 4402014800, i64* %29, align 4
  %32 = getelementptr i64, i64* %19, i64 7
  store i64 82, i64* %32, align 4
  %33 = getelementptr i64, i64* %19, i64 8
  store i64 %30, i64* %33, align 4
  %34 = getelementptr i64, i64* %19, i64 9
  store i64 %8, i64* %34, align 4
  %35 = getelementptr i64, i64* %19, i64 10
  store i64 ptrtoint (i64 (i64*)* @"55da4216-bb21-4904-8e49-cf19daa1e6d5" to i64), i64* %35, align 4
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
  store i64 4402017856, i64* %0, align 4
  store i64 %52, i64* %21, align 4
  store i64 %53, i64* %7, align 4
  store i64 %39, i64* %26, align 4
  store i64 %52, i64* %3, align 4
  %54 = musttail call i64 @e904a4d1-da3e-47ae-91f9-c4200c968fd6(i64* nonnull %0)
  ret i64 %54

pair_false:                                       ; preds = %cond1_true, %taken_false
  store i64 %38, i64* %21, align 4
  store i64 %39, i64* %26, align 4
  store i64 %38, i64* %3, align 4
  %55 = getelementptr i64, i64* %0, i64 5
  store i64 %12, i64* %55, align 4
  store i64 4402015984, i64* %0, align 4
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
define i64 @f58699b3-5194-4b6f-93e7-6bb290659cd4(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = load i64, i64* %3, align 4
  %5 = inttoptr i64 %4 to i64*
  store i64 %2, i64* %5, align 4
  %6 = add i64 %4, 8
  %7 = inttoptr i64 %6 to i64*
  store i64 4402013488, i64* %7, align 4
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
  store i64 ptrtoint (i64 (i64*)* @bcc11a66-4cf7-4d94-8d78-0b6ca647df24 to i64), i64* %15, align 4
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
  store i64 4402014000, i64* %0, align 4
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
  store i64 4402013568, i64* %0, align 4
  store i64 %51, i64* %9, align 4
  store i64 %52, i64* %12, align 4
  store i64 %21, i64* %16, align 4
  store i64 %51, i64* %3, align 4
  %53 = musttail call i64 @e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa(i64* nonnull %0)
  ret i64 %53

pair_false2:                                      ; preds = %cond1_true3, %pair_true
  store i64 %20, i64* %9, align 4
  store i64 %21, i64* %16, align 4
  store i64 %30, i64* %3, align 4
  store i64 4402013984, i64* %0, align 4
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
define i64 @"55da4216-bb21-4904-8e49-cf19daa1e6d5"(i64* noalias nocapture %0) #0 {
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
  store i64 4402014912, i64* %0, align 4
  %22 = tail call i64 inttoptr (i64 4358423904 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %15) #0
  store i64 %22, i64* %16, align 4
  %23 = icmp eq i64 %22, 66
  br i1 %23, label %undef_true, label %continue

pair_false:                                       ; preds = %cond1_true, %f9h_false
  store i64 4402014944, i64* %0, align 4
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
  store i64 4402013568, i64* %0, align 4
  %42 = getelementptr i64, i64* %0, i64 2
  store i64 %40, i64* %42, align 4
  store i64 %41, i64* %4, align 4
  store i64 %40, i64* %14, align 4
  %43 = musttail call i64 @e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa(i64* nonnull %0)
  ret i64 %43

pair_false2:                                      ; preds = %cond1_true3, %continue
  store i64 %21, i64* %14, align 4
  store i64 %22, i64* %1, align 4
  store i64 4402014880, i64* %0, align 4
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
define i64 @e904a4d1-da3e-47ae-91f9-c4200c968fd6(i64* noalias nocapture %0) local_unnamed_addr #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 7
  %2 = getelementptr i64, i64* %0, i64 6
  %3 = getelementptr i64, i64* %0, i64 3
  %4 = getelementptr i64, i64* %0, i64 2
  %.pre = load i64, i64* %2, align 4
  br label %tailrecurse

tailrecurse:                                      ; preds = %pair_true13, %entry
  %5 = phi i64 [ %72, %pair_true13 ], [ %.pre, %entry ]
  %6 = load i64, i64* %1, align 4
  %7 = add i64 %5, 104
  %8 = icmp ult i64 %7, %6
  br i1 %8, label %stack_ok, label %stack_overflow

stack_ok:                                         ; preds = %stack_overflow, %tailrecurse
  %9 = load i64, i64* %3, align 4
  %10 = add i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  %12 = getelementptr i64, i64* %11, i64 -1
  %13 = load i64, i64* %12, align 4
  %14 = icmp eq i64 %13, 50
  br i1 %14, label %taken_true, label %taken_false

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
  br i1 %17, label %cond1_true, label %pair_false

pair_true:                                        ; preds = %cond1_true
  %18 = load i64, i64* %2, align 4
  %19 = inttoptr i64 %18 to i64*
  store i64 %33, i64* %19, align 4
  %20 = add i64 %18, 8
  %21 = getelementptr i64, i64* %11, i64 -3
  %22 = load i64, i64* %21, align 4
  %23 = inttoptr i64 %20 to i64*
  store i64 %22, i64* %23, align 4
  %24 = add i64 %18, 16
  %25 = getelementptr i64, i64* %11, i64 -2
  %26 = load i64, i64* %25, align 4
  %27 = inttoptr i64 %24 to i64*
  store i64 %26, i64* %27, align 4
  %28 = add i64 %18, 24
  store i64 4402017776, i64* %0, align 4
  %29 = tail call i64 inttoptr (i64 4358466544 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %20) #0
  store i64 %29, i64* %23, align 4
  %30 = icmp eq i64 %29, 66
  br i1 %30, label %undef_true, label %continue

pair_false:                                       ; preds = %cond1_true, %taken_false
  %31 = getelementptr i64, i64* %0, i64 5
  store i64 18, i64* %31, align 4
  store i64 4402017824, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %13) #0
  ret i64 3

cond1_true:                                       ; preds = %taken_false
  %32 = inttoptr i64 %13 to i64*
  %33 = load i64, i64* %32, align 4
  %34 = and i64 %33, 15
  %35 = icmp eq i64 %34, 10
  br i1 %35, label %pair_false, label %pair_true

continue:                                         ; preds = %pair_true
  store i64 4402017760, i64* %0, align 4
  %36 = tail call i64 inttoptr (i64 4358462128 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %18) #0
  switch i64 %36, label %value_nonfalse [
    i64 66, label %undef_true2
    i64 34, label %value_false
  ]

undef_true:                                       ; preds = %pair_true
  %37 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %37, align 4
  ret i64 3

undef_true2:                                      ; preds = %continue
  %38 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %38, align 4
  ret i64 3

value_false:                                      ; preds = %continue
  %39 = load i64, i64* %12, align 4
  %40 = and i64 %39, 7
  %41 = icmp eq i64 %40, 0
  br i1 %41, label %cond1_true5, label %pair_false4

value_nonfalse:                                   ; preds = %continue
  %42 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %42, align 4
  ret i64 1

pair_true3:                                       ; preds = %cond1_true5
  store i64 %49, i64* %19, align 4
  %43 = load i64, i64* %21, align 4
  store i64 %43, i64* %23, align 4
  %44 = load i64, i64* %25, align 4
  store i64 %44, i64* %27, align 4
  store i64 4402017680, i64* %0, align 4
  %45 = tail call i64 inttoptr (i64 4358467104 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %20) #0
  store i64 %45, i64* %23, align 4
  %46 = icmp eq i64 %45, 66
  br i1 %46, label %undef_true7, label %continue6

pair_false4:                                      ; preds = %cond1_true5, %value_false
  store i64 %18, i64* %2, align 4
  %47 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %47, align 4
  store i64 4402017728, i64* %0, align 4
  tail call void @c_error_push_car_iloc(i64* nonnull %0, i64 %39) #0
  ret i64 3

cond1_true5:                                      ; preds = %value_false
  %48 = inttoptr i64 %39 to i64*
  %49 = load i64, i64* %48, align 4
  %50 = and i64 %49, 15
  %51 = icmp eq i64 %50, 10
  br i1 %51, label %pair_false4, label %pair_true3

continue6:                                        ; preds = %pair_true3
  store i64 4402017664, i64* %0, align 4
  %52 = tail call i64 inttoptr (i64 4358462128 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %18) #0
  switch i64 %52, label %value_nonfalse11 [
    i64 66, label %undef_true9
    i64 34, label %value_false10
  ]

undef_true7:                                      ; preds = %pair_true3
  %53 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %53, align 4
  ret i64 3

undef_true9:                                      ; preds = %continue6
  %54 = getelementptr i64, i64* %0, i64 5
  store i64 66, i64* %54, align 4
  ret i64 3

value_false10:                                    ; preds = %continue6
  %55 = load i64, i64* %21, align 4
  store i64 %55, i64* %19, align 4
  %56 = load i64, i64* %25, align 4
  %57 = and i64 %56, 1
  %58 = icmp eq i64 %57, 0
  br i1 %58, label %fallback, label %fixnum_true

value_nonfalse11:                                 ; preds = %continue6
  %59 = getelementptr i64, i64* %0, i64 5
  store i64 34, i64* %59, align 4
  ret i64 1

continue12:                                       ; preds = %fallback, %valid_true
  %.0 = phi i64 [ %67, %valid_true ], [ %65, %fallback ]
  store i64 %.0, i64* %23, align 4
  %60 = load i64, i64* %12, align 4
  %61 = and i64 %60, 7
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %cond1_true15, label %pair_false14

fixnum_true:                                      ; preds = %value_false10
  %63 = tail call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %56, i64 2)
  %64 = extractvalue { i64, i1 } %63, 1
  br i1 %64, label %fallback, label %valid_true

fallback:                                         ; preds = %fixnum_true, %value_false10
  store i64 %20, i64* %2, align 4
  %65 = tail call i64 @c_nadd_iloc(i64* nonnull %0, i64 4402018512) #0
  %66 = icmp eq i64 %65, 0
  br i1 %66, label %fallback_fail, label %continue12

valid_true:                                       ; preds = %fixnum_true
  %67 = extractvalue { i64, i1 } %63, 0
  br label %continue12

fallback_fail:                                    ; preds = %fallback
  ret i64 3

pair_true13:                                      ; preds = %cond1_true15
  %68 = getelementptr i64, i64* %75, i64 1
  %69 = load i64, i64* %68, align 4
  store i64 %69, i64* %27, align 4
  %70 = inttoptr i64 %28 to i64*
  store i64 3, i64* %70, align 4
  %71 = getelementptr i64, i64* %70, i64 1
  store i64 0, i64* %71, align 4
  %72 = add i64 %18, 40
  %73 = ptrtoint i64* %71 to i64
  store i64 4402017856, i64* %0, align 4
  store i64 %72, i64* %4, align 4
  store i64 %73, i64* %3, align 4
  store i64 %72, i64* %2, align 4
  br label %tailrecurse

pair_false14:                                     ; preds = %cond1_true15, %continue12
  store i64 %24, i64* %2, align 4
  %74 = getelementptr i64, i64* %0, i64 5
  store i64 %.0, i64* %74, align 4
  store i64 4402017584, i64* %0, align 4
  tail call void @c_error_push_cdr_iloc(i64* nonnull %0, i64 %60) #0
  ret i64 3

cond1_true15:                                     ; preds = %continue12
  %75 = inttoptr i64 %60 to i64*
  %76 = load i64, i64* %75, align 4
  %77 = and i64 %76, 15
  %78 = icmp eq i64 %77, 10
  br i1 %78, label %pair_false14, label %pair_true13
}

declare void @c_error_push_cdr_iloc(i64*, i64) local_unnamed_addr

; Function Attrs: nounwind
define i64 @bcc11a66-4cf7-4d94-8d78-0b6ca647df24(i64* noalias nocapture %0) #0 {
entry:
  %1 = getelementptr i64, i64* %0, i64 5
  %2 = load i64, i64* %1, align 4
  %3 = getelementptr i64, i64* %0, i64 6
  %4 = bitcast i64* %3 to i64**
  %5 = load i64*, i64** %4, align 4
  store i64 %2, i64* %5, align 4
  %6 = getelementptr i64, i64* %0, i64 2
  %7 = load i64, i64* %6, align 4
  store i64 4402013472, i64* %0, align 4
  %8 = tail call i64 inttoptr (i64 4358466544 to i64 (i64*, i64, i64)*)(i64* nonnull %0, i64 2, i64 %7) #0
  %9 = icmp eq i64 %8, 66
  store i64 %8, i64* %1, align 4
  %retval = select i1 %9, i64 3, i64 1
  ret i64 %retval
}

attributes #0 = { nounwind }
attributes #1 = { nounwind readnone speculatable willreturn }
