	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.intel_syntax noprefix
	.globl	"_27571fde-30ff-4ccb-96d7-18ad93155dfd" ## -- Begin function 27571fde-30ff-4ccb-96d7-18ad93155dfd
	.p2align	4, 0x90
"_27571fde-30ff-4ccb-96d7-18ad93155dfd": ## @"27571fde-30ff-4ccb-96d7-18ad93155dfd"
	.cfi_startproc
## %bb.0:                               ## %entry
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset rbx, -16
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [rax + 88]
	cmp	rcx, qword ptr [rdi + 56]
	jb	LBB0_2
## %bb.1:                               ## %stack_overflow
	mov	esi, 88
	mov	rdi, rbx
	call	_c_collect_stack
	mov	rax, qword ptr [rbx + 48]
LBB0_2:                                 ## %stack_ok
	movabs	rcx, 4460884064
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 82
	mov	rdx, qword ptr [rbx + 16]
	mov	qword ptr [rax + 16], rdx
	mov	rdx, qword ptr [rbx + 24]
	mov	qword ptr [rax + 24], rdx
	lea	rsi, [rip + "_f0cdbfb4-48b3-4909-961f-beb11e69318f"]
	mov	qword ptr [rax + 32], rsi
	mov	rsi, qword ptr [rbx + 32]
	mov	qword ptr [rax + 40], rsi
	mov	rdx, qword ptr [rdx - 16]
	mov	qword ptr [rax + 48], rdx
	mov	qword ptr [rax + 56], 1
	mov	qword ptr [rax + 64], 0
	lea	rdx, [rax + 40]
	lea	rsi, [rax + 64]
	add	rax, 72
	add	rcx, 7456
	mov	qword ptr [rbx], rcx
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], rsi
	mov	qword ptr [rbx + 32], rdx
	mov	qword ptr [rbx + 48], rax
	mov	rdi, rbx
	pop	rbx
	jmp	"_d9746434-c14a-41a4-8801-1a47e4bcc53a" ## TAILCALL
	.cfi_endproc
                                        ## -- End function
	.globl	"_f0cdbfb4-48b3-4909-961f-beb11e69318f" ## -- Begin function f0cdbfb4-48b3-4909-961f-beb11e69318f
	.p2align	4, 0x90
"_f0cdbfb4-48b3-4909-961f-beb11e69318f": ## @f0cdbfb4-48b3-4909-961f-beb11e69318f
## %bb.0:                               ## %entry
	mov	rax, qword ptr [rdi + 48]
	mov	rcx, qword ptr [rdi + 64]
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 50
	mov	qword ptr [rax + 16], 50
	mov	qword ptr [rax + 24], 3
	mov	qword ptr [rax + 32], 0
	lea	rcx, [rax + 32]
	add	rax, 40
	movabs	rdx, 4460888112
	mov	qword ptr [rdi], rdx
	mov	qword ptr [rdi + 16], rax
	mov	qword ptr [rdi + 24], rcx
	mov	qword ptr [rdi + 48], rax
	jmp	"_b8366db0-735c-45ff-90d6-64956277bc71" ## TAILCALL
                                        ## -- End function
	.globl	"_d9746434-c14a-41a4-8801-1a47e4bcc53a" ## -- Begin function d9746434-c14a-41a4-8801-1a47e4bcc53a
	.p2align	4, 0x90
"_d9746434-c14a-41a4-8801-1a47e4bcc53a": ## @d9746434-c14a-41a4-8801-1a47e4bcc53a
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [rax + 32]
	cmp	rcx, qword ptr [rdi + 56]
	jb	LBB2_2
## %bb.1:                               ## %stack_overflow
	mov	esi, 32
	mov	rdi, rbx
	call	_c_collect_stack
	mov	rax, qword ptr [rbx + 48]
	lea	rcx, [rax + 32]
LBB2_2:                                 ## %stack_ok
	mov	rdx, qword ptr [rbx + 24]
	mov	rdx, qword ptr [rdx - 16]
	mov	qword ptr [rax], rdx
	mov	qword ptr [rax + 8], 50
	mov	qword ptr [rax + 16], 2
	mov	qword ptr [rax + 24], 0
	add	rax, 24
	movabs	rdx, 4460892224
	mov	qword ptr [rbx], rdx
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_ef2ab2cd-1153-40c8-87d8-e7b9c12e2c91" ## TAILCALL
                                        ## -- End function
	.globl	"_ef2ab2cd-1153-40c8-87d8-e7b9c12e2c91" ## -- Begin function ef2ab2cd-1153-40c8-87d8-e7b9c12e2c91
	.p2align	4, 0x90
"_ef2ab2cd-1153-40c8-87d8-e7b9c12e2c91": ## @ef2ab2cd-1153-40c8-87d8-e7b9c12e2c91
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r13, rdi
	movabs	r12, 4460892224
	mov	rbx, qword ptr [rdi + 48]
	lea	r14, [r12 + 592]
	jmp	LBB3_1
	.p2align	4, 0x90
LBB3_16:                                ## %valid_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rbx, qword ptr [r13 + 48]
	mov	qword ptr [rbx], rax
	add	rbx, 8
	mov	qword ptr [r13 + 48], rbx
LBB3_17:                                ## %continue1
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rsi, qword ptr [r15 - 24]
	mov	qword ptr [rbx], rsi
	mov	r15, qword ptr [r15 - 16]
	mov	rdi, r13
	mov	rdx, r15
	call	_c_make_pair
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], 2
	mov	qword ptr [rbx + 16], 0
	lea	rax, [rbx + 16]
	add	rbx, 24
	mov	qword ptr [r13], r12
	mov	qword ptr [r13 + 16], rbx
	mov	qword ptr [r13 + 24], rax
	mov	qword ptr [r13 + 48], rbx
	mov	qword ptr [r13 + 64], r15
LBB3_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rbx, 32
	cmp	rbx, qword ptr [r13 + 56]
	jb	LBB3_2
## %bb.5:                               ## %stack_overflow
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	esi, 32
	mov	rdi, r13
	call	_c_collect_stack
LBB3_2:                                 ## %stack_ok
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	r15, qword ptr [r13 + 24]
	mov	rsi, qword ptr [r15 - 24]
	test	sil, 1
	jne	LBB3_6
## %bb.3:                               ## %nonfixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13], r12
	mov	edx, 1
	mov	rdi, r13
	call	_c_eq_n_iloc
	test	rax, rax
	jne	LBB3_9
## %bb.4:                               ## %continue
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	qword ptr [r13 + 64], 34
	je	LBB3_12
	jmp	LBB3_8
	.p2align	4, 0x90
LBB3_6:                                 ## %nonfixnum_false
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	rsi, 1
	je	LBB3_7
## %bb.11:                              ## %continue.thread
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13 + 64], 34
LBB3_12:                                ## %f9h_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rax, qword ptr [r15 - 24]
	test	al, 1
	je	LBB3_14
## %bb.13:                              ## %fixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	add	rax, -2
	jno	LBB3_16
LBB3_14:                                ## %fallback
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rdi, r13
	mov	rsi, r14
	call	_c_push_nadd_iloc
	test	rax, rax
	jne	LBB3_9
## %bb.15:                              ## %fallback.continue1_crit_edge
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rbx, qword ptr [r13 + 48]
	jmp	LBB3_17
LBB3_9:                                 ## %fallback_fail
	mov	eax, 3
	jmp	LBB3_10
LBB3_7:                                 ## %continue.thread8
	mov	qword ptr [r13 + 64], 18
LBB3_8:                                 ## %f9h_false
	mov	rax, qword ptr [r15 - 16]
	cmp	rax, 66
	mov	qword ptr [r13 + 64], rax
	mov	ecx, 7
	mov	eax, 1
	cmove	rax, rcx
LBB3_10:                                ## %fallback_fail
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_b8366db0-735c-45ff-90d6-64956277bc71" ## -- Begin function b8366db0-735c-45ff-90d6-64956277bc71
	.p2align	4, 0x90
"_b8366db0-735c-45ff-90d6-64956277bc71": ## @b8366db0-735c-45ff-90d6-64956277bc71
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	eax, 152
	add	rax, qword ptr [rdi + 48]
	cmp	rax, qword ptr [rdi + 56]
	jb	LBB4_1
## %bb.10:                              ## %stack_overflow
	mov	esi, 152
	mov	rdi, rbx
	call	_c_collect_stack
LBB4_1:                                 ## %stack_ok
	mov	rdi, qword ptr [rbx + 24]
	mov	r10, qword ptr [rdi - 32]
	cmp	r10, 50
	jne	LBB4_7
## %bb.2:                               ## %taken_true
	cmp	qword ptr [rdi - 24], 50
	jne	LBB4_5
## %bb.3:                               ## %taken_true1
	mov	qword ptr [rbx + 64], 3
	jmp	LBB4_4
LBB4_7:                                 ## %taken_false
	movabs	rcx, 4460885376
	mov	rdx, qword ptr [rbx + 48]
	lea	rsi, [rcx + 2688]
	mov	qword ptr [rdx], rsi
	mov	qword ptr [rdx + 8], 82
	mov	rsi, qword ptr [rbx + 16]
	mov	qword ptr [rdx + 16], rsi
	mov	qword ptr [rdx + 24], rdi
	lea	rsi, [rip + "_d8c8fc56-3059-4286-8623-bda7b6bec6e1"]
	mov	qword ptr [rdx + 32], rsi
	mov	rsi, qword ptr [rbx + 32]
	lea	r8, [rdx + 40]
	mov	qword ptr [rdx + 40], rsi
	lea	rsi, [rdx + 48]
	lea	rax, [rcx + 3968]
	mov	qword ptr [rdx + 48], rax
	mov	qword ptr [rdx + 56], 82
	mov	qword ptr [rdx + 64], rsi
	mov	qword ptr [rdx + 72], rdi
	lea	rax, [rip + "_a3d338a2-71f9-423e-89f5-89a1becf552a"]
	mov	qword ptr [rdx + 80], rax
	mov	qword ptr [rdx + 88], r8
	lea	r8, [rdx + 88]
	add	rdx, 96
	mov	rsi, qword ptr [rdi - 32]
	test	sil, 7
	jne	LBB4_6
## %bb.8:                               ## %cond1_true
	mov	r9, qword ptr [rsi]
	mov	eax, r9d
	and	eax, 15
	cmp	eax, 10
	jne	LBB4_9
LBB4_6:                                 ## %pair_false
	mov	qword ptr [rbx + 16], rdx
	mov	qword ptr [rbx + 32], r8
	mov	qword ptr [rbx + 48], rdx
	mov	qword ptr [rbx + 64], r10
	add	rcx, 5152
	mov	qword ptr [rbx], rcx
	mov	rdi, rbx
	call	_c_error_push_car_iloc
	mov	eax, 3
	pop	rbx
	ret
LBB4_5:                                 ## %taken_false2
	mov	qword ptr [rbx + 64], 1
LBB4_4:                                 ## %taken_true1
	mov	eax, 1
	pop	rbx
	ret
LBB4_9:                                 ## %pair_true
	mov	qword ptr [rdx], r9
	mov	qword ptr [rdx + 8], 3
	mov	rax, qword ptr [rdi - 16]
	mov	qword ptr [rdx + 16], rax
	mov	qword ptr [rdx + 24], 3
	mov	qword ptr [rdx + 32], 0
	lea	rax, [rdx + 32]
	add	rdx, 40
	mov	qword ptr [rbx], rcx
	mov	qword ptr [rbx + 16], rdx
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 32], r8
	mov	qword ptr [rbx + 48], rdx
	mov	qword ptr [rbx + 64], r10
	mov	rdi, rbx
	pop	rbx
	jmp	"_a519f28c-0fb0-402e-8620-987c185a6aad" ## TAILCALL
                                        ## -- End function
	.globl	"_d8c8fc56-3059-4286-8623-bda7b6bec6e1" ## -- Begin function d8c8fc56-3059-4286-8623-bda7b6bec6e1
	.p2align	4, 0x90
"_d8c8fc56-3059-4286-8623-bda7b6bec6e1": ## @d8c8fc56-3059-4286-8623-bda7b6bec6e1
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	push	rax
	mov	r12, rdi
	movabs	r15, 4460887920
	mov	rbx, qword ptr [rdi + 48]
	mov	rax, qword ptr [rdi + 64]
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], r15
	mov	qword ptr [rbx + 16], 82
	mov	rax, qword ptr [rdi + 16]
	mov	qword ptr [rbx + 24], rax
	mov	rbp, qword ptr [rdi + 24]
	mov	qword ptr [rbx + 32], rbp
	lea	rax, [rip + "_4ecc0264-2762-438f-8464-688487aa2b31"]
	mov	qword ptr [rbx + 40], rax
	mov	rax, qword ptr [rdi + 32]
	mov	qword ptr [rbx + 48], rax
	lea	r13, [rbx + 48]
	add	rbx, 56
	mov	rsi, qword ptr [rbp - 32]
	test	sil, 7
	jne	LBB5_1
## %bb.3:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	jne	LBB5_4
LBB5_1:                                 ## %pair_false
	mov	qword ptr [r12 + 16], rbx
	mov	qword ptr [r12 + 32], r13
	mov	qword ptr [r12 + 48], rbx
	add	r15, 624
	mov	qword ptr [r12], r15
	mov	rdi, r12
	call	_c_error_push_cdr_iloc
LBB5_2:                                 ## %pair_false
	mov	eax, 3
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
LBB5_4:                                 ## %pair_true
	mov	rax, qword ptr [rsi + 8]
	mov	qword ptr [rbx], rax
	mov	rsi, qword ptr [rbp - 32]
	test	sil, 7
	jne	LBB5_5
## %bb.6:                               ## %cond1_true3
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	jne	LBB5_7
LBB5_5:                                 ## %pair_false2
	mov	qword ptr [r12 + 16], rbx
	add	rbx, 8
	mov	qword ptr [r12 + 32], r13
	mov	qword ptr [r12 + 48], rbx
	add	r15, 608
	mov	qword ptr [r12], r15
	mov	rdi, r12
	call	_c_error_push_car_iloc
	jmp	LBB5_2
LBB5_7:                                 ## %pair_true1
	mov	qword ptr [rbx + 8], rax
	mov	r14, qword ptr [rbp - 24]
	mov	rdi, r12
	mov	rsi, rax
	mov	rdx, r14
	call	_c_make_pair
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [rbp - 16]
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], 3
	mov	qword ptr [rbx + 32], 0
	lea	rax, [rbx + 32]
	add	rbx, 40
	add	r15, 192
	mov	qword ptr [r12], r15
	mov	qword ptr [r12 + 16], rbx
	mov	qword ptr [r12 + 24], rax
	mov	qword ptr [r12 + 32], r13
	mov	qword ptr [r12 + 48], rbx
	mov	qword ptr [r12 + 64], r14
	mov	rdi, r12
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	jmp	"_b8366db0-735c-45ff-90d6-64956277bc71" ## TAILCALL
                                        ## -- End function
	.globl	"_a3d338a2-71f9-423e-89f5-89a1becf552a" ## -- Begin function a3d338a2-71f9-423e-89f5-89a1becf552a
	.p2align	4, 0x90
"_a3d338a2-71f9-423e-89f5-89a1becf552a": ## @a3d338a2-71f9-423e-89f5-89a1becf552a
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	push	rax
	mov	rbx, rdi
	cmp	qword ptr [rdi + 64], 34
	jne	LBB6_3
## %bb.1:                               ## %f9h_true
	mov	qword ptr [rbx + 64], 1
	mov	eax, 1
	jmp	LBB6_10
LBB6_3:                                 ## %f9h_false
	mov	r15, qword ptr [rbx + 24]
	mov	rsi, qword ptr [r15 - 32]
	test	sil, 7
	jne	LBB6_2
## %bb.4:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	jne	LBB6_5
LBB6_2:                                 ## %pair_false
	movabs	rax, 4364473488
	add	rax, 96416000
	mov	qword ptr [rbx], rax
	mov	rdi, rbx
	call	_c_error_push_cdr_iloc
LBB6_9:                                 ## %undef_true
	mov	eax, 3
LBB6_10:                                ## %undef_true
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
LBB6_5:                                 ## %pair_true
	movabs	r12, 4364473488
	mov	rax, qword ptr [rsi + 8]
	mov	r14, qword ptr [rbx + 48]
	mov	qword ptr [r14], rax
	lea	rbp, [r14 + 8]
	mov	rax, qword ptr [r15 - 24]
	mov	qword ptr [r14 + 8], rax
	lea	r13, [r14 + 16]
	lea	rax, [r12 + 96415968]
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 48], r13
	mov	esi, 2
	mov	rdi, rbx
	mov	rdx, r14
	call	r12
	mov	qword ptr [r14], rax
	cmp	rax, 66
	jne	LBB6_6
## %bb.8:                               ## %undef_true
	mov	qword ptr [rbx + 64], 66
	mov	qword ptr [rbx + 48], rbp
	jmp	LBB6_9
LBB6_6:                                 ## %continue
	mov	qword ptr [rbp], 50
	mov	rsi, qword ptr [r15 - 32]
	test	sil, 7
	jne	LBB6_7
## %bb.11:                              ## %cond1_true3
	mov	rcx, qword ptr [rsi]
	mov	edx, ecx
	and	edx, 15
	cmp	edx, 10
	jne	LBB6_12
LBB6_7:                                 ## %pair_false2
	mov	qword ptr [rbx + 48], r13
	mov	qword ptr [rbx + 64], rax
	add	r12, 96415936
	mov	qword ptr [rbx], r12
	mov	rdi, rbx
	call	_c_error_push_car_iloc
	jmp	LBB6_9
LBB6_12:                                ## %pair_true1
	mov	qword ptr [r14 + 16], rcx
	mov	r15, qword ptr [r15 - 16]
	mov	rdi, rbx
	mov	rsi, rcx
	mov	rdx, r15
	call	_c_make_pair
	mov	qword ptr [r14 + 16], rax
	mov	qword ptr [r14 + 24], 3
	mov	qword ptr [r14 + 32], 0
	lea	rax, [r14 + 32]
	add	r14, 40
	add	r12, 96414624
	mov	qword ptr [rbx], r12
	mov	qword ptr [rbx + 16], r14
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 48], r14
	mov	qword ptr [rbx + 64], r15
	mov	rdi, rbx
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	jmp	"_b8366db0-735c-45ff-90d6-64956277bc71" ## TAILCALL
                                        ## -- End function
	.globl	"_a519f28c-0fb0-402e-8620-987c185a6aad" ## -- Begin function a519f28c-0fb0-402e-8620-987c185a6aad
	.p2align	4, 0x90
"_a519f28c-0fb0-402e-8620-987c185a6aad": ## @a519f28c-0fb0-402e-8620-987c185a6aad
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 40
	mov	r14, rdi
	movabs	rbp, 4364510432
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [rbp + 96374864]
	mov	qword ptr [rsp + 24], rcx ## 8-byte Spill
	lea	rcx, [rbp + 4416]
	mov	qword ptr [rsp + 16], rcx ## 8-byte Spill
LBB7_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rax, 104
	cmp	rax, qword ptr [r14 + 56]
	jb	LBB7_2
## %bb.29:                              ## %stack_overflow
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	esi, 104
	mov	rdi, r14
	call	_c_collect_stack
LBB7_2:                                 ## %stack_ok
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	r13, qword ptr [r14 + 24]
	mov	rsi, qword ptr [r13 - 16]
	cmp	rsi, 50
	je	LBB7_3
## %bb.8:                               ## %taken_false
                                        ##   in Loop: Header=BB7_1 Depth=1
	test	sil, 7
	jne	LBB7_6
## %bb.9:                               ## %cond1_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_6
## %bb.10:                              ## %pair_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rbx, qword ptr [r14 + 48]
	mov	qword ptr [rbx], rax
	lea	rdx, [rbx + 8]
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	lea	r15, [rbx + 16]
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	lea	r12, [rbx + 24]
	mov	rax, qword ptr [rsp + 24] ## 8-byte Reload
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 48], r12
	mov	qword ptr [r14 + 64], rsi
	mov	esi, 2
	mov	rdi, r14
	mov	qword ptr [rsp + 32], rdx ## 8-byte Spill
	call	qword ptr [rsp + 16]    ## 8-byte Folded Reload
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_25
## %bb.11:                              ## %continue
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rcx, [rbp + 96374848]
	mov	qword ptr [r14], rcx
	mov	qword ptr [rsp + 8], r15 ## 8-byte Spill
	mov	qword ptr [r14 + 48], r15
	mov	qword ptr [r14 + 64], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	rbp
	cmp	rax, 34
	jne	LBB7_12
## %bb.15:                              ## %value_false
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_14
## %bb.16:                              ## %cond1_true5
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_14
## %bb.17:                              ## %pair_true3
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	lea	rax, [rbp + 96374768]
	mov	qword ptr [r14], rax
	mov	qword ptr [r14 + 48], r12
	mov	qword ptr [r14 + 64], 34
	lea	rax, [rbp + 4976]
	mov	esi, 2
	mov	rdi, r14
	mov	r12, qword ptr [rsp + 32] ## 8-byte Reload
	mov	rdx, r12
	call	rax
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_30
## %bb.18:                              ## %continue6
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rcx, [rbp + 96374752]
	mov	qword ptr [r14], rcx
	mov	r15, qword ptr [rsp + 8] ## 8-byte Reload
	mov	qword ptr [r14 + 48], r15
	mov	qword ptr [r14 + 64], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	rbp
	cmp	rax, 34
	jne	LBB7_12
## %bb.19:                              ## %value_false10
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 24]
	test	al, 1
	je	LBB7_21
## %bb.20:                              ## %fixnum_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	add	rax, 2
	jno	LBB7_22
LBB7_21:                                ## %fallback
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r14 + 48], r12
	mov	qword ptr [r14 + 64], 34
	lea	rsi, [rbp + 96375600]
	mov	rdi, r14
	call	_c_push_nadd_iloc
	test	rax, rax
	je	LBB7_23
	jmp	LBB7_26
LBB7_22:                                ## %valid_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r12], rax
	mov	qword ptr [r14 + 48], r15
LBB7_23:                                ## %continue12
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_24
## %bb.27:                              ## %cond1_true15
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	je	LBB7_24
## %bb.28:                              ## %pair_true13
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rcx, qword ptr [rsi + 8]
	mov	rax, qword ptr [r14 + 48]
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 3
	mov	qword ptr [rax + 16], 0
	lea	rcx, [rax + 16]
	add	rax, 24
	lea	rdx, [rbp + 96374944]
	mov	qword ptr [r14], rdx
	mov	qword ptr [r14 + 16], rax
	mov	qword ptr [r14 + 24], rcx
	mov	qword ptr [r14 + 48], rax
	mov	qword ptr [r14 + 64], 34
	jmp	LBB7_1
LBB7_6:                                 ## %pair_false
	mov	qword ptr [r14 + 64], rsi
	add	rbp, 96374912
	jmp	LBB7_7
LBB7_14:                                ## %pair_false4
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 64], 34
	add	rbp, 96374816
LBB7_7:                                 ## %pair_false
	mov	qword ptr [r14], rbp
	mov	rdi, r14
	call	_c_error_push_car_iloc
	mov	eax, 3
	jmp	LBB7_5
LBB7_12:                                ## %continue
	cmp	rax, 66
	jne	LBB7_31
## %bb.13:                              ## %undef_true2
	mov	qword ptr [r14 + 64], 66
	mov	qword ptr [r14 + 48], rbx
	mov	eax, 3
	jmp	LBB7_5
LBB7_24:                                ## %pair_false14
	mov	qword ptr [r14 + 64], 34
	add	rbp, 96374672
	mov	qword ptr [r14], rbp
	mov	rdi, r14
	call	_c_error_push_cdr_iloc
	mov	eax, 3
	jmp	LBB7_5
LBB7_3:                                 ## %taken_true
	mov	qword ptr [r14 + 64], 18
	jmp	LBB7_4
LBB7_25:                                ## %undef_true
	mov	qword ptr [r14 + 64], 66
	mov	qword ptr [r14 + 48], r15
LBB7_26:                                ## %fallback_fail
	mov	eax, 3
	jmp	LBB7_5
LBB7_30:                                ## %undef_true7
	mov	qword ptr [r14 + 64], 66
	mov	rax, qword ptr [rsp + 8] ## 8-byte Reload
	mov	qword ptr [r14 + 48], rax
	mov	eax, 3
	jmp	LBB7_5
LBB7_31:                                ## %value_nonfalse
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 64], 34
LBB7_4:                                 ## %taken_true
	mov	eax, 1
LBB7_5:                                 ## %taken_true
	add	rsp, 40
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
                                        ## -- End function
	.globl	"_4ecc0264-2762-438f-8464-688487aa2b31" ## -- Begin function 4ecc0264-2762-438f-8464-688487aa2b31
	.p2align	4, 0x90
"_4ecc0264-2762-438f-8464-688487aa2b31": ## @"4ecc0264-2762-438f-8464-688487aa2b31"
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 48]
	mov	rcx, qword ptr [rdi + 64]
	mov	qword ptr [rax], rcx
	add	rax, 8
	mov	rdx, qword ptr [rdi + 16]
	movabs	rcx, 4364514848
	lea	rsi, [rcx + 96373056]
	mov	qword ptr [rdi], rsi
	mov	qword ptr [rdi + 48], rax
	mov	esi, 2
	call	rcx
	xor	ecx, ecx
	cmp	rax, 66
	sete	cl
	mov	qword ptr [rbx + 64], rax
	lea	rax, [rcx + rcx + 1]
	pop	rbx
	ret
                                        ## -- End function
.subsections_via_symbols
