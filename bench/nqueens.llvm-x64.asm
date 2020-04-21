	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.intel_syntax noprefix
	.globl	"_8fb2aece-298c-423e-884a-5512a5083426" ## -- Begin function 8fb2aece-298c-423e-884a-5512a5083426
	.p2align	4, 0x90
"_8fb2aece-298c-423e-884a-5512a5083426": ## @"8fb2aece-298c-423e-884a-5512a5083426"
	.cfi_startproc
## %bb.0:                               ## %entry
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset rbx, -16
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [rax + 88]
	cmp	rcx, qword ptr [rdi + 56]
	jae	LBB0_1
LBB0_2:                                 ## %stack_ok
	movabs	rcx, 4635770064
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 82
	mov	rdx, qword ptr [rbx + 16]
	mov	qword ptr [rax + 16], rdx
	mov	rdx, qword ptr [rbx + 24]
	mov	qword ptr [rax + 24], rdx
	lea	rsi, [rip + "_c774198f-eb0c-4775-8000-7e4d93de65c0"]
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
	add	rcx, 74912
	mov	qword ptr [rbx], rcx
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], rsi
	mov	qword ptr [rbx + 32], rdx
	mov	qword ptr [rbx + 48], rax
	mov	rdi, rbx
	pop	rbx
	jmp	"_482ea6ea-0a04-4860-9252-5ec2c53905d4" ## TAILCALL
LBB0_1:                                 ## %stack_overflow
	mov	esi, 88
	mov	rdi, rbx
	call	_c_collect_stack
	mov	rax, qword ptr [rbx + 48]
	jmp	LBB0_2
	.cfi_endproc
                                        ## -- End function
	.globl	"_c774198f-eb0c-4775-8000-7e4d93de65c0" ## -- Begin function c774198f-eb0c-4775-8000-7e4d93de65c0
	.p2align	4, 0x90
"_c774198f-eb0c-4775-8000-7e4d93de65c0": ## @c774198f-eb0c-4775-8000-7e4d93de65c0
## %bb.0:                               ## %entry
	mov	rax, qword ptr [rdi + 40]
	mov	rcx, qword ptr [rdi + 48]
	mov	qword ptr [rcx], rax
	mov	qword ptr [rcx + 8], 50
	mov	qword ptr [rcx + 16], 50
	mov	qword ptr [rcx + 24], 3
	mov	qword ptr [rcx + 32], 0
	lea	rax, [rcx + 32]
	add	rcx, 40
	movabs	rdx, 4635848480
	mov	qword ptr [rdi], rdx
	mov	qword ptr [rdi + 16], rcx
	mov	qword ptr [rdi + 24], rax
	mov	qword ptr [rdi + 48], rcx
	jmp	"_5796dc35-449b-4518-8560-d1c4ae1c07ee" ## TAILCALL
                                        ## -- End function
	.globl	"_482ea6ea-0a04-4860-9252-5ec2c53905d4" ## -- Begin function 482ea6ea-0a04-4860-9252-5ec2c53905d4
	.p2align	4, 0x90
"_482ea6ea-0a04-4860-9252-5ec2c53905d4": ## @"482ea6ea-0a04-4860-9252-5ec2c53905d4"
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [rax + 32]
	cmp	rcx, qword ptr [rdi + 56]
	jae	LBB2_1
LBB2_2:                                 ## %stack_ok
	mov	rcx, qword ptr [rbx + 24]
	mov	rcx, qword ptr [rcx - 16]
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 50
	mov	qword ptr [rax + 16], 2
	mov	qword ptr [rax + 24], 0
	lea	rcx, [rax + 24]
	add	rax, 32
	movabs	rdx, 4635851952
	mov	qword ptr [rbx], rdx
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], rcx
	mov	qword ptr [rbx + 48], rax
	mov	rdi, rbx
	pop	rbx
	jmp	"_03c70879-9207-4716-9682-b22d5eaf7ea2" ## TAILCALL
LBB2_1:                                 ## %stack_overflow
	mov	esi, 32
	mov	rdi, rbx
	call	_c_collect_stack
	mov	rax, qword ptr [rbx + 48]
	jmp	LBB2_2
                                        ## -- End function
	.globl	"_03c70879-9207-4716-9682-b22d5eaf7ea2" ## -- Begin function 03c70879-9207-4716-9682-b22d5eaf7ea2
	.p2align	4, 0x90
"_03c70879-9207-4716-9682-b22d5eaf7ea2": ## @"03c70879-9207-4716-9682-b22d5eaf7ea2"
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r13, rdi
	movabs	r15, 4635851952
	mov	rbx, qword ptr [rdi + 48]
	lea	r14, [r15 + 592]
	.p2align	4, 0x90
LBB3_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rbx, 32
	cmp	rbx, qword ptr [r13 + 56]
	jae	LBB3_13
LBB3_2:                                 ## %stack_ok
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	r12, qword ptr [r13 + 24]
	mov	rax, qword ptr [r12 - 24]
	test	al, 1
	je	LBB3_3
## %bb.10:                              ## %nonfixnum_false
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	rax, 1
	je	LBB3_11
LBB3_6:                                 ## %fixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	add	rax, -2
	jo	LBB3_7
LBB3_12:                                ## %continue1
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rbx, qword ptr [r13 + 48]
	mov	qword ptr [rbx], rax
	mov	rsi, qword ptr [r12 - 24]
	mov	qword ptr [rbx + 8], rsi
	mov	rdx, qword ptr [r12 - 16]
	mov	rdi, r13
	call	_c_make_pair
	mov	qword ptr [rbx + 8], rax
	mov	qword ptr [rbx + 16], 2
	mov	qword ptr [rbx + 24], 0
	lea	rax, [rbx + 24]
	add	rbx, 32
	mov	qword ptr [r13], r15
	mov	qword ptr [r13 + 16], rbx
	mov	qword ptr [r13 + 24], rax
	mov	qword ptr [r13 + 48], rbx
	jmp	LBB3_1
LBB3_13:                                ## %stack_overflow
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	esi, 32
	mov	rdi, r13
	call	_c_collect_stack
	jmp	LBB3_2
LBB3_3:                                 ## %nonfixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13], r15
	mov	edx, 1
	mov	rdi, r13
	mov	rsi, rax
	call	_c_eq_n_iloc
	test	rax, rax
	je	LBB3_8
## %bb.4:                               ## %nonfixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	rax, 34
	jne	LBB3_11
## %bb.5:                               ## %f9h_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rax, qword ptr [r12 - 24]
	test	al, 1
	jne	LBB3_6
LBB3_7:                                 ## %fallback
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rdi, r13
	mov	rsi, r14
	call	_c_nadd_iloc
	test	rax, rax
	jne	LBB3_12
LBB3_8:                                 ## %fallback_fail
	mov	eax, 3
	jmp	LBB3_9
LBB3_11:                                ## %f9h_false
	mov	rax, qword ptr [r12 - 16]
	mov	qword ptr [r13 + 40], rax
	mov	eax, 1
LBB3_9:                                 ## %fallback_fail
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_5796dc35-449b-4518-8560-d1c4ae1c07ee" ## -- Begin function 5796dc35-449b-4518-8560-d1c4ae1c07ee
	.p2align	4, 0x90
"_5796dc35-449b-4518-8560-d1c4ae1c07ee": ## @"5796dc35-449b-4518-8560-d1c4ae1c07ee"
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	eax, 152
	add	rax, qword ptr [rdi + 48]
	cmp	rax, qword ptr [rdi + 56]
	jae	LBB4_10
LBB4_1:                                 ## %stack_ok
	mov	rdi, qword ptr [rbx + 24]
	mov	r8, qword ptr [rdi - 32]
	cmp	r8, 50
	je	LBB4_2
## %bb.7:                               ## %taken_false
	movabs	r10, 4635845856
	mov	rcx, qword ptr [rbx + 48]
	lea	rdx, [r10 + 2576]
	mov	qword ptr [rcx], rdx
	mov	qword ptr [rcx + 8], 82
	mov	rdx, qword ptr [rbx + 16]
	mov	qword ptr [rcx + 16], rdx
	mov	qword ptr [rcx + 24], rdi
	lea	rdx, [rip + "_003d62b3-9080-4271-9955-57819026bc0f"]
	mov	qword ptr [rcx + 32], rdx
	mov	rdx, qword ptr [rbx + 32]
	lea	rsi, [rcx + 40]
	mov	qword ptr [rcx + 40], rdx
	lea	rdx, [rcx + 48]
	lea	rax, [r10 + 3856]
	mov	qword ptr [rcx + 48], rax
	mov	qword ptr [rcx + 56], 82
	mov	qword ptr [rcx + 64], rdx
	mov	qword ptr [rcx + 72], rdi
	lea	rax, [rip + "_44ac696d-0a2f-4be5-850f-7815bab27706"]
	mov	qword ptr [rcx + 80], rax
	lea	r9, [rcx + 88]
	mov	qword ptr [rcx + 88], rsi
	mov	rsi, qword ptr [rdi - 32]
	test	sil, 7
	jne	LBB4_6
## %bb.8:                               ## %cond1_true
	mov	rdx, qword ptr [rsi]
	mov	eax, edx
	and	eax, 15
	cmp	eax, 10
	je	LBB4_6
## %bb.9:                               ## %pair_true
	mov	qword ptr [rcx + 96], rdx
	mov	qword ptr [rcx + 104], 3
	mov	rax, qword ptr [rdi - 16]
	mov	qword ptr [rcx + 112], rax
	mov	qword ptr [rcx + 120], 3
	lea	rax, [rcx + 128]
	mov	qword ptr [rcx + 128], 0
	add	rcx, 136
	mov	qword ptr [rbx], r10
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 32], r9
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_86821bb4-da31-4486-8544-d06da7fb5300" ## TAILCALL
LBB4_6:                                 ## %pair_false
	add	rcx, 96
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 32], r9
	mov	qword ptr [rbx + 48], rcx
	mov	qword ptr [rbx + 40], r8
	add	r10, 5040
	mov	qword ptr [rbx], r10
	mov	rdi, rbx
	call	_c_error_push_car_iloc
	mov	eax, 3
	pop	rbx
	ret
LBB4_10:                                ## %stack_overflow
	mov	esi, 152
	mov	rdi, rbx
	call	_c_collect_stack
	jmp	LBB4_1
LBB4_2:                                 ## %taken_true
	cmp	qword ptr [rdi - 24], 50
	je	LBB4_3
## %bb.5:                               ## %taken_false2
	mov	qword ptr [rbx + 40], 1
LBB4_4:                                 ## %taken_true1
	mov	eax, 1
	pop	rbx
	ret
LBB4_3:                                 ## %taken_true1
	mov	qword ptr [rbx + 40], 3
	jmp	LBB4_4
                                        ## -- End function
	.globl	"_003d62b3-9080-4271-9955-57819026bc0f" ## -- Begin function 003d62b3-9080-4271-9955-57819026bc0f
	.p2align	4, 0x90
"_003d62b3-9080-4271-9955-57819026bc0f": ## @"003d62b3-9080-4271-9955-57819026bc0f"
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r15, rdi
	movabs	r14, 4635848400
	mov	rax, qword ptr [rdi + 40]
	mov	rbx, qword ptr [rdi + 48]
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], r14
	mov	qword ptr [rbx + 16], 82
	mov	rax, qword ptr [rdi + 16]
	mov	qword ptr [rbx + 24], rax
	mov	r13, qword ptr [rdi + 24]
	mov	qword ptr [rbx + 32], r13
	lea	rax, [rip + "_3f297490-673d-43f4-8aad-524975092d89"]
	mov	qword ptr [rbx + 40], rax
	mov	rax, qword ptr [rdi + 32]
	lea	r12, [rbx + 48]
	mov	qword ptr [rbx + 48], rax
	lea	rcx, [rbx + 56]
	mov	rsi, qword ptr [r13 - 32]
	test	sil, 7
	jne	LBB5_1
## %bb.3:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	je	LBB5_1
## %bb.4:                               ## %pair_true
	mov	rax, qword ptr [rsi + 8]
	mov	qword ptr [rbx + 56], rax
	mov	rsi, qword ptr [r13 - 32]
	test	sil, 7
	jne	LBB5_5
## %bb.6:                               ## %cond1_true3
	mov	rax, qword ptr [rsi]
	mov	edx, eax
	and	edx, 15
	cmp	edx, 10
	je	LBB5_5
## %bb.7:                               ## %pair_true1
	mov	qword ptr [rbx + 64], rax
	mov	rdx, qword ptr [r13 - 24]
	mov	rdi, r15
	mov	rsi, rax
	call	_c_make_pair
	mov	qword ptr [rbx + 64], rax
	mov	rax, qword ptr [r13 - 16]
	mov	qword ptr [rbx + 72], rax
	mov	qword ptr [rbx + 80], 3
	lea	rax, [rbx + 88]
	mov	qword ptr [rbx + 88], 0
	add	rbx, 96
	add	r14, 80
	mov	qword ptr [r15], r14
	mov	qword ptr [r15 + 16], rbx
	mov	qword ptr [r15 + 24], rax
	mov	qword ptr [r15 + 32], r12
	mov	qword ptr [r15 + 48], rbx
	mov	rdi, r15
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	jmp	"_5796dc35-449b-4518-8560-d1c4ae1c07ee" ## TAILCALL
LBB5_1:                                 ## %pair_false
	mov	qword ptr [r15 + 16], rcx
	mov	qword ptr [r15 + 32], r12
	mov	qword ptr [r15 + 48], rcx
	add	r14, 512
	mov	qword ptr [r15], r14
	mov	rdi, r15
	call	_c_error_push_cdr_iloc
	jmp	LBB5_2
LBB5_5:                                 ## %pair_false2
	add	rbx, 64
	mov	qword ptr [r15 + 16], rcx
	mov	qword ptr [r15 + 32], r12
	mov	qword ptr [r15 + 48], rbx
	add	r14, 496
	mov	qword ptr [r15], r14
	mov	rdi, r15
	call	_c_error_push_car_iloc
LBB5_2:                                 ## %pair_false
	mov	eax, 3
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_44ac696d-0a2f-4be5-850f-7815bab27706" ## -- Begin function 44ac696d-0a2f-4be5-850f-7815bab27706
	.p2align	4, 0x90
"_44ac696d-0a2f-4be5-850f-7815bab27706": ## @"44ac696d-0a2f-4be5-850f-7815bab27706"
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r12
	push	rbx
	push	rax
	mov	r14, rdi
	cmp	qword ptr [rdi + 40], 34
	je	LBB6_1
## %bb.3:                               ## %f9h_false
	movabs	r15, 4532076672
	mov	r12, qword ptr [r14 + 24]
	mov	rsi, qword ptr [r12 - 32]
	test	sil, 7
	jne	LBB6_2
## %bb.4:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	je	LBB6_2
## %bb.5:                               ## %pair_true
	mov	rax, qword ptr [rsi + 8]
	mov	rbx, qword ptr [r14 + 48]
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r12 - 24]
	mov	qword ptr [rbx + 8], rax
	lea	rax, [r15 + 103773152]
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r15
	mov	qword ptr [rbx], rax
	cmp	rax, 66
	je	LBB6_8
## %bb.6:                               ## %continue
	mov	qword ptr [rbx + 8], 50
	mov	rsi, qword ptr [r12 - 32]
	test	sil, 7
	jne	LBB6_7
## %bb.11:                              ## %cond1_true3
	mov	rcx, qword ptr [rsi]
	mov	edx, ecx
	and	edx, 15
	cmp	edx, 10
	je	LBB6_7
## %bb.12:                              ## %pair_true1
	mov	qword ptr [rbx + 16], rcx
	mov	rdx, qword ptr [r12 - 16]
	mov	rdi, r14
	mov	rsi, rcx
	call	_c_make_pair
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], 3
	lea	rax, [rbx + 32]
	mov	qword ptr [rbx + 32], 0
	add	rbx, 40
	add	r15, 103771808
	mov	qword ptr [r14], r15
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 24], rax
	mov	qword ptr [r14 + 48], rbx
	mov	rdi, r14
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	jmp	"_5796dc35-449b-4518-8560-d1c4ae1c07ee" ## TAILCALL
LBB6_2:                                 ## %pair_false
	add	r15, 103773184
	mov	qword ptr [r14], r15
	mov	rdi, r14
	call	_c_error_push_cdr_iloc
	jmp	LBB6_9
LBB6_7:                                 ## %pair_false2
	add	rbx, 16
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], rax
	add	r15, 103773120
	mov	qword ptr [r14], r15
	mov	rdi, r14
	call	_c_error_push_car_iloc
	jmp	LBB6_9
LBB6_1:                                 ## %f9h_true
	mov	qword ptr [r14 + 40], 1
	mov	eax, 1
	jmp	LBB6_10
LBB6_8:                                 ## %undef_true
	mov	qword ptr [r14 + 40], 66
LBB6_9:                                 ## %undef_true
	mov	eax, 3
LBB6_10:                                ## %undef_true
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_86821bb4-da31-4486-8544-d06da7fb5300" ## -- Begin function 86821bb4-da31-4486-8544-d06da7fb5300
	.p2align	4, 0x90
"_86821bb4-da31-4486-8544-d06da7fb5300": ## @"86821bb4-da31-4486-8544-d06da7fb5300"
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 24
	mov	r14, rdi
	mov	rbx, qword ptr [rdi + 48]
	movabs	r12, 4532114896
	lea	rdx, [r12 + 103730880]
	lea	rbp, [r12 + 4416]
	lea	rax, [r12 + 103730864]
	mov	qword ptr [rsp + 16], rax ## 8-byte Spill
LBB7_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rbx, 104
	cmp	rbx, qword ptr [r14 + 56]
	jae	LBB7_29
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
	lea	r15, [rbx + 8]
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rsp + 8], rdx ## 8-byte Spill
	mov	qword ptr [r14], rdx
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, r15
	mov	qword ptr [rsp], rbp    ## 8-byte Spill
	call	rbp
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_25
## %bb.11:                              ## %continue
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsp + 16] ## 8-byte Reload
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
	cmp	rax, 66
	je	LBB7_25
## %bb.12:                              ## %continue
                                        ##   in Loop: Header=BB7_1 Depth=1
	cmp	rax, 34
	jne	LBB7_24
## %bb.13:                              ## %value_false
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_14
## %bb.15:                              ## %cond1_true5
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_14
## %bb.16:                              ## %pair_true3
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	lea	rax, [r12 + 103730784]
	mov	qword ptr [r14], rax
	lea	rax, [r12 + 4976]
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, r15
	call	rax
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_25
## %bb.17:                              ## %continue6
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rax, [r12 + 103730768]
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
	cmp	rax, 66
	je	LBB7_25
## %bb.18:                              ## %continue6
                                        ##   in Loop: Header=BB7_1 Depth=1
	cmp	rax, 34
	jne	LBB7_24
## %bb.19:                              ## %value_false10
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 24]
	test	al, 1
	jne	LBB7_20
LBB7_21:                                ## %fallback
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r14 + 48], r15
	lea	rsi, [r12 + 103731616]
	mov	rdi, r14
	call	_c_nadd_iloc
	test	rax, rax
	jne	LBB7_22
	jmp	LBB7_26
LBB7_29:                                ## %stack_overflow
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	esi, 104
	mov	rdi, r14
	mov	rbx, rdx
	call	_c_collect_stack
	mov	rdx, rbx
	jmp	LBB7_2
LBB7_20:                                ## %fixnum_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	add	rax, 2
	jo	LBB7_21
LBB7_22:                                ## %continue12
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r15], rax
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_23
## %bb.27:                              ## %cond1_true15
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	ecx, dword ptr [rsi]
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_23
## %bb.28:                              ## %pair_true13
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi + 8]
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], 3
	lea	rax, [rbx + 32]
	mov	qword ptr [rbx + 32], 0
	add	rbx, 40
	lea	rcx, [r12 + 103730960]
	mov	qword ptr [r14], rcx
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 24], rax
	mov	qword ptr [r14 + 48], rbx
	mov	rdx, qword ptr [rsp + 8] ## 8-byte Reload
	mov	rbp, qword ptr [rsp]    ## 8-byte Reload
	jmp	LBB7_1
LBB7_24:                                ## %value_nonfalse
	mov	qword ptr [r14 + 40], 34
LBB7_4:                                 ## %taken_true
	mov	eax, 1
LBB7_5:                                 ## %taken_true
	add	rsp, 24
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
LBB7_6:                                 ## %pair_false
	mov	qword ptr [r14 + 40], 18
	add	r12, 103730928
LBB7_7:                                 ## %pair_false
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_car_iloc
	mov	eax, 3
	jmp	LBB7_5
LBB7_25:                                ## %undef_true
	mov	qword ptr [r14 + 40], 66
LBB7_26:                                ## %fallback_fail
	mov	eax, 3
	jmp	LBB7_5
LBB7_3:                                 ## %taken_true
	mov	qword ptr [r14 + 40], 18
	jmp	LBB7_4
LBB7_14:                                ## %pair_false4
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], 34
	add	r12, 103730832
	jmp	LBB7_7
LBB7_23:                                ## %pair_false14
	add	rbx, 16
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], rax
	add	r12, 103730688
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_cdr_iloc
	mov	eax, 3
	jmp	LBB7_5
                                        ## -- End function
	.globl	"_3f297490-673d-43f4-8aad-524975092d89" ## -- Begin function 3f297490-673d-43f4-8aad-524975092d89
	.p2align	4, 0x90
"_3f297490-673d-43f4-8aad-524975092d89": ## @"3f297490-673d-43f4-8aad-524975092d89"
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 40]
	mov	rcx, qword ptr [rdi + 48]
	mov	qword ptr [rcx], rax
	mov	rdx, qword ptr [rdi + 16]
	movabs	rax, 4532119312
	lea	rcx, [rax + 103729072]
	mov	qword ptr [rdi], rcx
	mov	esi, 2
	call	rax
	cmp	rax, 66
	mov	qword ptr [rbx + 40], rax
	je	LBB8_1
## %bb.2:                               ## %select.false
	mov	eax, 1
	pop	rbx
	ret
LBB8_1:
	mov	eax, 3
	pop	rbx
	ret
                                        ## -- End function
.subsections_via_symbols
