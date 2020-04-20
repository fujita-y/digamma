	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.intel_syntax noprefix
	.globl	"_48606894-c232-44a1-835f-2a253e06472e" ## -- Begin function 48606894-c232-44a1-835f-2a253e06472e
	.p2align	4, 0x90
"_48606894-c232-44a1-835f-2a253e06472e": ## @"48606894-c232-44a1-835f-2a253e06472e"
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
	movabs	rcx, 4401937424
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 82
	mov	rdx, qword ptr [rbx + 16]
	mov	qword ptr [rax + 16], rdx
	mov	rdx, qword ptr [rbx + 24]
	mov	qword ptr [rax + 24], rdx
	lea	rsi, [rip + "_bf8534cd-c97d-489d-8941-1e597392dba3"]
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
	add	rcx, 79552
	mov	qword ptr [rbx], rcx
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], rsi
	mov	qword ptr [rbx + 32], rdx
	mov	qword ptr [rbx + 48], rax
	mov	rdi, rbx
	pop	rbx
	jmp	"_47d922f7-5ea0-486b-94da-c9dc0977b2c1" ## TAILCALL
	.cfi_endproc
                                        ## -- End function
	.globl	"_bf8534cd-c97d-489d-8941-1e597392dba3" ## -- Begin function bf8534cd-c97d-489d-8941-1e597392dba3
	.p2align	4, 0x90
"_bf8534cd-c97d-489d-8941-1e597392dba3": ## @bf8534cd-c97d-489d-8941-1e597392dba3
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
	movabs	rdx, 4402013568
	mov	qword ptr [rdi], rdx
	mov	qword ptr [rdi + 16], rcx
	mov	qword ptr [rdi + 24], rax
	mov	qword ptr [rdi + 48], rcx
	jmp	"_e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa" ## TAILCALL
                                        ## -- End function
	.globl	"_47d922f7-5ea0-486b-94da-c9dc0977b2c1" ## -- Begin function 47d922f7-5ea0-486b-94da-c9dc0977b2c1
	.p2align	4, 0x90
"_47d922f7-5ea0-486b-94da-c9dc0977b2c1": ## @"47d922f7-5ea0-486b-94da-c9dc0977b2c1"
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
	movabs	rdx, 4402012400
	mov	qword ptr [rbx], rdx
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_9a10efff-cc4c-4d72-99c6-4efd419644d9" ## TAILCALL
                                        ## -- End function
	.globl	"_9a10efff-cc4c-4d72-99c6-4efd419644d9" ## -- Begin function 9a10efff-cc4c-4d72-99c6-4efd419644d9
	.p2align	4, 0x90
"_9a10efff-cc4c-4d72-99c6-4efd419644d9": ## @"9a10efff-cc4c-4d72-99c6-4efd419644d9"
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r13, rdi
	movabs	r15, 4402012400
	mov	rbx, qword ptr [rdi + 48]
	lea	r14, [r15 + 592]
	jmp	LBB3_1
	.p2align	4, 0x90
LBB3_11:                                ## %fallback
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rdi, r13
	mov	rsi, r14
	call	_c_nadd_iloc
	test	rax, rax
	je	LBB3_5
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
LBB3_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rbx, 32
	cmp	rbx, qword ptr [r13 + 56]
	jb	LBB3_2
## %bb.13:                              ## %stack_overflow
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	esi, 32
	mov	rdi, r13
	call	_c_collect_stack
LBB3_2:                                 ## %stack_ok
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	r12, qword ptr [r13 + 24]
	mov	rax, qword ptr [r12 - 24]
	test	al, 1
	jne	LBB3_7
## %bb.3:                               ## %nonfixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13], r15
	mov	edx, 1
	mov	rdi, r13
	mov	rsi, rax
	call	_c_eq_n_iloc
	cmp	rax, 34
	jne	LBB3_4
## %bb.9:                               ## %f9h_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rax, qword ptr [r12 - 24]
	test	al, 1
	je	LBB3_11
	jmp	LBB3_10
	.p2align	4, 0x90
LBB3_7:                                 ## %nonfixnum_false
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	rax, 1
	je	LBB3_8
LBB3_10:                                ## %fixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	add	rax, -2
	jo	LBB3_11
	jmp	LBB3_12
LBB3_4:                                 ## %nonfixnum_true
	test	rax, rax
	jne	LBB3_8
LBB3_5:                                 ## %fallback_fail
	mov	eax, 3
	jmp	LBB3_6
LBB3_8:                                 ## %f9h_false
	mov	rax, qword ptr [r12 - 16]
	mov	qword ptr [r13 + 40], rax
	mov	eax, 1
LBB3_6:                                 ## %fallback_fail
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa" ## -- Begin function e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa
	.p2align	4, 0x90
"_e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa": ## @e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa
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
	mov	rdx, qword ptr [rbx + 24]
	mov	r10, qword ptr [rdx - 32]
	cmp	r10, 50
	jne	LBB4_7
## %bb.2:                               ## %taken_true
	cmp	qword ptr [rdx - 24], 50
	jne	LBB4_5
## %bb.3:                               ## %taken_true1
	mov	qword ptr [rbx + 40], 3
	jmp	LBB4_4
LBB4_7:                                 ## %taken_false
	movabs	rax, 4402013520
	mov	rcx, qword ptr [rbx + 48]
	mov	qword ptr [rcx], rax
	mov	qword ptr [rcx + 8], 82
	mov	rsi, qword ptr [rbx + 16]
	mov	qword ptr [rcx + 16], rsi
	mov	qword ptr [rcx + 24], rdx
	lea	rsi, [rip + "_f58699b3-5194-4b6f-93e7-6bb290659cd4"]
	mov	qword ptr [rcx + 32], rsi
	mov	rsi, qword ptr [rbx + 32]
	lea	r8, [rcx + 40]
	mov	qword ptr [rcx + 40], rsi
	lea	rsi, [rcx + 48]
	lea	rdi, [rax + 1280]
	mov	qword ptr [rcx + 48], rdi
	mov	qword ptr [rcx + 56], 82
	mov	qword ptr [rcx + 64], rsi
	mov	qword ptr [rcx + 72], rdx
	lea	rsi, [rip + "_55da4216-bb21-4904-8e49-cf19daa1e6d5"]
	mov	qword ptr [rcx + 80], rsi
	mov	qword ptr [rcx + 88], r8
	lea	r8, [rcx + 88]
	add	rcx, 96
	mov	rsi, qword ptr [rdx - 32]
	test	sil, 7
	jne	LBB4_6
## %bb.8:                               ## %cond1_true
	mov	r9, qword ptr [rsi]
	mov	edi, r9d
	and	edi, 15
	cmp	edi, 10
	jne	LBB4_9
LBB4_6:                                 ## %pair_false
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 32], r8
	mov	qword ptr [rbx + 48], rcx
	mov	qword ptr [rbx + 40], r10
	add	rax, 2464
	mov	qword ptr [rbx], rax
	mov	rdi, rbx
	call	_c_error_push_car_iloc
	mov	eax, 3
	pop	rbx
	ret
LBB4_5:                                 ## %taken_false2
	mov	qword ptr [rbx + 40], 1
LBB4_4:                                 ## %taken_true1
	mov	eax, 1
	pop	rbx
	ret
LBB4_9:                                 ## %pair_true
	mov	qword ptr [rcx], r9
	mov	qword ptr [rcx + 8], 3
	mov	rdx, qword ptr [rdx - 16]
	mov	qword ptr [rcx + 16], rdx
	mov	qword ptr [rcx + 24], 3
	mov	qword ptr [rcx + 32], 0
	lea	rdx, [rcx + 32]
	add	rcx, 40
	add	rax, 4336
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rdx
	mov	qword ptr [rbx + 32], r8
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_e904a4d1-da3e-47ae-91f9-c4200c968fd6" ## TAILCALL
                                        ## -- End function
	.globl	"_f58699b3-5194-4b6f-93e7-6bb290659cd4" ## -- Begin function f58699b3-5194-4b6f-93e7-6bb290659cd4
	.p2align	4, 0x90
"_f58699b3-5194-4b6f-93e7-6bb290659cd4": ## @f58699b3-5194-4b6f-93e7-6bb290659cd4
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r15, rdi
	movabs	r14, 4402013488
	mov	rax, qword ptr [rdi + 40]
	mov	rbx, qword ptr [rdi + 48]
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], r14
	mov	qword ptr [rbx + 16], 82
	mov	rax, qword ptr [rdi + 16]
	mov	qword ptr [rbx + 24], rax
	mov	r13, qword ptr [rdi + 24]
	mov	qword ptr [rbx + 32], r13
	lea	rax, [rip + "_bcc11a66-4cf7-4d94-8d78-0b6ca647df24"]
	mov	qword ptr [rbx + 40], rax
	mov	rax, qword ptr [rdi + 32]
	mov	qword ptr [rbx + 48], rax
	lea	r12, [rbx + 48]
	add	rbx, 56
	mov	rsi, qword ptr [r13 - 32]
	test	sil, 7
	jne	LBB5_1
## %bb.3:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	jne	LBB5_4
LBB5_1:                                 ## %pair_false
	mov	qword ptr [r15 + 16], rbx
	mov	qword ptr [r15 + 32], r12
	mov	qword ptr [r15 + 48], rbx
	add	r14, 512
	mov	qword ptr [r15], r14
	mov	rdi, r15
	call	_c_error_push_cdr_iloc
LBB5_2:                                 ## %pair_false
	mov	eax, 3
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
LBB5_4:                                 ## %pair_true
	mov	rax, qword ptr [rsi + 8]
	mov	qword ptr [rbx], rax
	mov	rsi, qword ptr [r13 - 32]
	test	sil, 7
	jne	LBB5_5
## %bb.6:                               ## %cond1_true3
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	jne	LBB5_7
LBB5_5:                                 ## %pair_false2
	mov	qword ptr [r15 + 16], rbx
	add	rbx, 8
	mov	qword ptr [r15 + 32], r12
	mov	qword ptr [r15 + 48], rbx
	add	r14, 496
	mov	qword ptr [r15], r14
	mov	rdi, r15
	call	_c_error_push_car_iloc
	jmp	LBB5_2
LBB5_7:                                 ## %pair_true1
	mov	qword ptr [rbx + 8], rax
	mov	rdx, qword ptr [r13 - 24]
	mov	rdi, r15
	mov	rsi, rax
	call	_c_make_pair
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 16]
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], 3
	mov	qword ptr [rbx + 32], 0
	lea	rax, [rbx + 32]
	add	rbx, 40
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
	jmp	"_e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa" ## TAILCALL
                                        ## -- End function
	.globl	"_55da4216-bb21-4904-8e49-cf19daa1e6d5" ## -- Begin function 55da4216-bb21-4904-8e49-cf19daa1e6d5
	.p2align	4, 0x90
"_55da4216-bb21-4904-8e49-cf19daa1e6d5": ## @"55da4216-bb21-4904-8e49-cf19daa1e6d5"
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r12
	push	rbx
	push	rax
	mov	rbx, rdi
	cmp	qword ptr [rdi + 40], 34
	jne	LBB6_3
## %bb.1:                               ## %f9h_true
	mov	qword ptr [rbx + 40], 1
	mov	eax, 1
	jmp	LBB6_10
LBB6_3:                                 ## %f9h_false
	mov	r12, qword ptr [rbx + 24]
	mov	rsi, qword ptr [r12 - 32]
	test	sil, 7
	jne	LBB6_2
## %bb.4:                               ## %cond1_true
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	jne	LBB6_5
LBB6_2:                                 ## %pair_false
	movabs	rax, 4358423904
	add	rax, 43591040
	mov	qword ptr [rbx], rax
	mov	rdi, rbx
	call	_c_error_push_cdr_iloc
LBB6_9:                                 ## %undef_true
	mov	eax, 3
LBB6_10:                                ## %undef_true
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	ret
LBB6_5:                                 ## %pair_true
	movabs	r15, 4358423904
	mov	rax, qword ptr [rsi + 8]
	mov	r14, qword ptr [rbx + 48]
	mov	qword ptr [r14], rax
	mov	rax, qword ptr [r12 - 24]
	mov	qword ptr [r14 + 8], rax
	lea	rax, [r15 + 43591008]
	mov	qword ptr [rbx], rax
	mov	esi, 2
	mov	rdi, rbx
	mov	rdx, r14
	call	r15
	mov	qword ptr [r14], rax
	cmp	rax, 66
	jne	LBB6_6
## %bb.8:                               ## %undef_true
	mov	qword ptr [rbx + 40], 66
	jmp	LBB6_9
LBB6_6:                                 ## %continue
	mov	qword ptr [r14 + 8], 50
	mov	rsi, qword ptr [r12 - 32]
	test	sil, 7
	jne	LBB6_7
## %bb.11:                              ## %cond1_true3
	mov	rcx, qword ptr [rsi]
	mov	edx, ecx
	and	edx, 15
	cmp	edx, 10
	jne	LBB6_12
LBB6_7:                                 ## %pair_false2
	add	r14, 16
	mov	qword ptr [rbx + 48], r14
	mov	qword ptr [rbx + 40], rax
	add	r15, 43590976
	mov	qword ptr [rbx], r15
	mov	rdi, rbx
	call	_c_error_push_car_iloc
	jmp	LBB6_9
LBB6_12:                                ## %pair_true1
	mov	qword ptr [r14 + 16], rcx
	mov	rdx, qword ptr [r12 - 16]
	mov	rdi, rbx
	mov	rsi, rcx
	call	_c_make_pair
	mov	qword ptr [r14 + 16], rax
	mov	qword ptr [r14 + 24], 3
	lea	rax, [r14 + 32]
	mov	qword ptr [r14 + 32], 0
	add	r14, 40
	add	r15, 43589664
	mov	qword ptr [rbx], r15
	mov	qword ptr [rbx + 16], r14
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 48], r14
	mov	rdi, rbx
	add	rsp, 8
	pop	rbx
	pop	r12
	pop	r14
	pop	r15
	jmp	"_e0da4e4c-b0ed-4cd5-84ca-e75dfcc365aa" ## TAILCALL
                                        ## -- End function
	.globl	"_e904a4d1-da3e-47ae-91f9-c4200c968fd6" ## -- Begin function e904a4d1-da3e-47ae-91f9-c4200c968fd6
	.p2align	4, 0x90
"_e904a4d1-da3e-47ae-91f9-c4200c968fd6": ## @e904a4d1-da3e-47ae-91f9-c4200c968fd6
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 24
	mov	r14, rdi
	movabs	r12, 4358462128
	mov	rbx, qword ptr [rdi + 48]
	lea	rax, [r12 + 43555648]
	mov	qword ptr [rsp + 16], rax ## 8-byte Spill
	lea	rbp, [r12 + 4416]
	lea	rax, [r12 + 43555632]
	mov	qword ptr [rsp + 8], rax ## 8-byte Spill
LBB7_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rbx, 104
	cmp	rbx, qword ptr [r14 + 56]
	jb	LBB7_2
## %bb.27:                              ## %stack_overflow
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
	lea	r15, [rbx + 8]
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	mov	rax, qword ptr [rsp + 16] ## 8-byte Reload
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, r15
	call	rbp
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_28
## %bb.11:                              ## %continue
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsp + 8] ## 8-byte Reload
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
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
	lea	rax, [r12 + 43555552]
	mov	qword ptr [r14], rax
	lea	rax, [r12 + 4976]
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, r15
	call	rax
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_28
## %bb.18:                              ## %continue6
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rax, [r12 + 43555536]
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
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
	mov	qword ptr [r14 + 48], r15
	lea	rsi, [r12 + 43556384]
	mov	rdi, r14
	call	_c_nadd_iloc
	test	rax, rax
	je	LBB7_24
LBB7_22:                                ## %continue12
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r15], rax
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_23
## %bb.25:                              ## %cond1_true15
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	ecx, dword ptr [rsi]
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_23
## %bb.26:                              ## %pair_true13
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rax, [rbx + 24]
	mov	rcx, qword ptr [rsi + 8]
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], 3
	add	rax, 8
	mov	qword ptr [rbx + 32], 0
	add	rbx, 40
	lea	rcx, [r12 + 43555728]
	mov	qword ptr [r14], rcx
	mov	qword ptr [r14 + 16], rbx
	mov	qword ptr [r14 + 24], rax
	mov	qword ptr [r14 + 48], rbx
	jmp	LBB7_1
LBB7_6:                                 ## %pair_false
	mov	qword ptr [r14 + 40], 18
	add	r12, 43555696
	jmp	LBB7_7
LBB7_14:                                ## %pair_false4
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], 34
	add	r12, 43555600
LBB7_7:                                 ## %pair_false
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_car_iloc
	mov	eax, 3
	jmp	LBB7_5
LBB7_12:                                ## %continue
	cmp	rax, 66
	jne	LBB7_13
LBB7_28:                                ## %undef_true
	mov	qword ptr [r14 + 40], 66
	mov	eax, 3
	jmp	LBB7_5
LBB7_23:                                ## %pair_false14
	add	rbx, 16
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], rax
	add	r12, 43555456
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_cdr_iloc
LBB7_24:                                ## %fallback_fail
	mov	eax, 3
	jmp	LBB7_5
LBB7_3:                                 ## %taken_true
	mov	qword ptr [r14 + 40], 18
	jmp	LBB7_4
LBB7_13:                                ## %value_nonfalse
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
                                        ## -- End function
	.globl	"_bcc11a66-4cf7-4d94-8d78-0b6ca647df24" ## -- Begin function bcc11a66-4cf7-4d94-8d78-0b6ca647df24
	.p2align	4, 0x90
"_bcc11a66-4cf7-4d94-8d78-0b6ca647df24": ## @bcc11a66-4cf7-4d94-8d78-0b6ca647df24
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 40]
	mov	rcx, qword ptr [rdi + 48]
	mov	qword ptr [rcx], rax
	mov	rdx, qword ptr [rdi + 16]
	movabs	rax, 4358466544
	lea	rcx, [rax + 43546928]
	mov	qword ptr [rdi], rcx
	mov	esi, 2
	call	rax
	xor	ecx, ecx
	cmp	rax, 66
	sete	cl
	mov	qword ptr [rbx + 40], rax
	lea	rax, [rcx + rcx + 1]
	pop	rbx
	ret
                                        ## -- End function
.subsections_via_symbols
