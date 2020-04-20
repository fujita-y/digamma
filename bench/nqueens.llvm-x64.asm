	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.intel_syntax noprefix
	.globl	"_b7745e8e-f182-4b26-8f07-785454198eea" ## -- Begin function b7745e8e-f182-4b26-8f07-785454198eea
	.p2align	4, 0x90
"_b7745e8e-f182-4b26-8f07-785454198eea": ## @b7745e8e-f182-4b26-8f07-785454198eea
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
	movabs	rcx, 4526800128
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 82
	mov	rdx, qword ptr [rbx + 16]
	mov	qword ptr [rax + 16], rdx
	mov	rdx, qword ptr [rbx + 24]
	mov	qword ptr [rax + 24], rdx
	lea	rsi, [rip + "_5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93"]
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
	add	rcx, 82368
	mov	qword ptr [rbx], rcx
	mov	qword ptr [rbx + 16], rax
	mov	qword ptr [rbx + 24], rsi
	mov	qword ptr [rbx + 32], rdx
	mov	qword ptr [rbx + 48], rax
	mov	rdi, rbx
	pop	rbx
	jmp	"_2c696dce-7d4f-4b08-9e9c-58af994c2399" ## TAILCALL
	.cfi_endproc
                                        ## -- End function
	.globl	"_5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93" ## -- Begin function 5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93
	.p2align	4, 0x90
"_5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93": ## @"5d1629d8-b071-4bc3-8d2e-0c4a3bb78e93"
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
	movabs	rdx, 4526877904
	mov	qword ptr [rdi], rdx
	mov	qword ptr [rdi + 16], rcx
	mov	qword ptr [rdi + 24], rax
	mov	qword ptr [rdi + 48], rcx
	jmp	"_051cca58-8c2c-49b9-9a86-c080a588ede6" ## TAILCALL
                                        ## -- End function
	.globl	"_2c696dce-7d4f-4b08-9e9c-58af994c2399" ## -- Begin function 2c696dce-7d4f-4b08-9e9c-58af994c2399
	.p2align	4, 0x90
"_2c696dce-7d4f-4b08-9e9c-58af994c2399": ## @"2c696dce-7d4f-4b08-9e9c-58af994c2399"
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
	movabs	rdx, 4526881376
	mov	qword ptr [rbx], rdx
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rax
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_a86247ed-3932-42fe-8b12-f6f7e6ad581c" ## TAILCALL
                                        ## -- End function
	.globl	"_a86247ed-3932-42fe-8b12-f6f7e6ad581c" ## -- Begin function a86247ed-3932-42fe-8b12-f6f7e6ad581c
	.p2align	4, 0x90
"_a86247ed-3932-42fe-8b12-f6f7e6ad581c": ## @a86247ed-3932-42fe-8b12-f6f7e6ad581c
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r13, rdi
	movabs	r15, 4526881376
	mov	rbx, qword ptr [rdi + 48]
	lea	r14, [r15 + 592]
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
	mov	rsi, qword ptr [r12 - 24]
	mov	qword ptr [rbx], rsi
	mov	rdx, qword ptr [r12 - 16]
	mov	rdi, r13
	call	_c_make_pair
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], 2
	mov	qword ptr [rbx + 16], 0
	lea	rax, [rbx + 16]
	add	rbx, 24
	mov	qword ptr [r13], r15
	mov	qword ptr [r13 + 16], rbx
	mov	qword ptr [r13 + 24], rax
	mov	qword ptr [r13 + 48], rbx
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
	mov	r12, qword ptr [r13 + 24]
	mov	rsi, qword ptr [r12 - 24]
	test	sil, 1
	jne	LBB3_6
## %bb.3:                               ## %nonfixnum_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13], r15
	mov	edx, 1
	mov	rdi, r13
	call	_c_eq_n_iloc
	test	rax, rax
	jne	LBB3_9
## %bb.4:                               ## %continue
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	qword ptr [r13 + 40], 34
	je	LBB3_12
	jmp	LBB3_8
	.p2align	4, 0x90
LBB3_6:                                 ## %nonfixnum_false
                                        ##   in Loop: Header=BB3_1 Depth=1
	cmp	rsi, 1
	je	LBB3_7
## %bb.11:                              ## %continue.thread
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	qword ptr [r13 + 40], 34
LBB3_12:                                ## %f9h_true
                                        ##   in Loop: Header=BB3_1 Depth=1
	mov	rax, qword ptr [r12 - 24]
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
	mov	qword ptr [r13 + 40], 18
LBB3_8:                                 ## %f9h_false
	mov	rax, qword ptr [r12 - 16]
	mov	qword ptr [r13 + 40], rax
	mov	eax, 1
LBB3_10:                                ## %fallback_fail
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	ret
                                        ## -- End function
	.globl	"_051cca58-8c2c-49b9-9a86-c080a588ede6" ## -- Begin function 051cca58-8c2c-49b9-9a86-c080a588ede6
	.p2align	4, 0x90
"_051cca58-8c2c-49b9-9a86-c080a588ede6": ## @"051cca58-8c2c-49b9-9a86-c080a588ede6"
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
	movabs	rax, 4526875280
	mov	rcx, qword ptr [rbx + 48]
	lea	rsi, [rax + 2576]
	mov	qword ptr [rcx], rsi
	mov	qword ptr [rcx + 8], 82
	mov	rsi, qword ptr [rbx + 16]
	mov	qword ptr [rcx + 16], rsi
	mov	qword ptr [rcx + 24], rdx
	lea	rsi, [rip + "_d070c580-04b2-414c-9f5a-37ef01b58089"]
	mov	qword ptr [rcx + 32], rsi
	mov	rsi, qword ptr [rbx + 32]
	lea	r8, [rcx + 40]
	mov	qword ptr [rcx + 40], rsi
	lea	rsi, [rcx + 48]
	lea	rdi, [rax + 3856]
	mov	qword ptr [rcx + 48], rdi
	mov	qword ptr [rcx + 56], 82
	mov	qword ptr [rcx + 64], rsi
	mov	qword ptr [rcx + 72], rdx
	lea	rsi, [rip + "_44d6386f-e8e4-4381-9c0f-5ad845cc8da7"]
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
	add	rax, 5040
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
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 16], rcx
	mov	qword ptr [rbx + 24], rdx
	mov	qword ptr [rbx + 32], r8
	mov	qword ptr [rbx + 48], rcx
	mov	rdi, rbx
	pop	rbx
	jmp	"_b2c68e4c-59b7-4e71-9792-323c589decc3" ## TAILCALL
                                        ## -- End function
	.globl	"_d070c580-04b2-414c-9f5a-37ef01b58089" ## -- Begin function d070c580-04b2-414c-9f5a-37ef01b58089
	.p2align	4, 0x90
"_d070c580-04b2-414c-9f5a-37ef01b58089": ## @d070c580-04b2-414c-9f5a-37ef01b58089
## %bb.0:                               ## %entry
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	mov	r15, rdi
	movabs	r14, 4526877824
	mov	rax, qword ptr [rdi + 40]
	mov	rbx, qword ptr [rdi + 48]
	mov	qword ptr [rbx], rax
	mov	qword ptr [rbx + 8], r14
	mov	qword ptr [rbx + 16], 82
	mov	rax, qword ptr [rdi + 16]
	mov	qword ptr [rbx + 24], rax
	mov	r13, qword ptr [rdi + 24]
	mov	qword ptr [rbx + 32], r13
	lea	rax, [rip + "_2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2"]
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
	jmp	"_051cca58-8c2c-49b9-9a86-c080a588ede6" ## TAILCALL
                                        ## -- End function
	.globl	"_44d6386f-e8e4-4381-9c0f-5ad845cc8da7" ## -- Begin function 44d6386f-e8e4-4381-9c0f-5ad845cc8da7
	.p2align	4, 0x90
"_44d6386f-e8e4-4381-9c0f-5ad845cc8da7": ## @"44d6386f-e8e4-4381-9c0f-5ad845cc8da7"
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
	movabs	rax, 4404351056
	add	rax, 122528224
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
	movabs	r15, 4404351056
	mov	rax, qword ptr [rsi + 8]
	mov	r14, qword ptr [rbx + 48]
	mov	qword ptr [r14], rax
	mov	rax, qword ptr [r12 - 24]
	mov	qword ptr [r14 + 8], rax
	lea	rax, [r15 + 122528192]
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
	add	r15, 122528160
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
	add	r15, 122526848
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
	jmp	"_051cca58-8c2c-49b9-9a86-c080a588ede6" ## TAILCALL
                                        ## -- End function
	.globl	"_b2c68e4c-59b7-4e71-9792-323c589decc3" ## -- Begin function b2c68e4c-59b7-4e71-9792-323c589decc3
	.p2align	4, 0x90
"_b2c68e4c-59b7-4e71-9792-323c589decc3": ## @b2c68e4c-59b7-4e71-9792-323c589decc3
## %bb.0:                               ## %entry
	push	rbp
	push	r15
	push	r14
	push	r13
	push	r12
	push	rbx
	sub	rsp, 24
	mov	r14, rdi
	movabs	r12, 4404389280
	mov	rax, qword ptr [rdi + 48]
	lea	rcx, [r12 + 122485920]
	mov	qword ptr [rsp + 16], rcx ## 8-byte Spill
	lea	rbp, [r12 + 4416]
	lea	rcx, [r12 + 122485904]
	mov	qword ptr [rsp + 8], rcx ## 8-byte Spill
LBB7_1:                                 ## %tailrecurse
                                        ## =>This Inner Loop Header: Depth=1
	add	rax, 104
	cmp	rax, qword ptr [r14 + 56]
	jb	LBB7_3
## %bb.2:                               ## %stack_overflow
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	esi, 104
	mov	rdi, r14
	call	_c_collect_stack
LBB7_3:                                 ## %stack_ok
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	r13, qword ptr [r14 + 24]
	mov	rsi, qword ptr [r13 - 16]
	cmp	rsi, 50
	je	LBB7_25
## %bb.4:                               ## %taken_false
                                        ##   in Loop: Header=BB7_1 Depth=1
	test	sil, 7
	jne	LBB7_19
## %bb.5:                               ## %cond1_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_19
## %bb.6:                               ## %pair_true
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
	je	LBB7_23
## %bb.7:                               ## %continue
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsp + 8] ## 8-byte Reload
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
	cmp	rax, 34
	jne	LBB7_22
## %bb.8:                               ## %value_false
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_20
## %bb.9:                               ## %cond1_true5
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [rsi]
	mov	ecx, eax
	and	ecx, 15
	cmp	ecx, 10
	je	LBB7_20
## %bb.10:                              ## %pair_true3
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx + 8], rax
	mov	rax, qword ptr [r13 - 24]
	mov	qword ptr [rbx + 16], rax
	lea	rax, [r12 + 122485824]
	mov	qword ptr [r14], rax
	lea	rax, [r12 + 4976]
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, r15
	call	rax
	mov	qword ptr [rbx + 8], rax
	cmp	rax, 66
	je	LBB7_23
## %bb.11:                              ## %continue6
                                        ##   in Loop: Header=BB7_1 Depth=1
	lea	rax, [r12 + 122485808]
	mov	qword ptr [r14], rax
	mov	esi, 2
	mov	rdi, r14
	mov	rdx, rbx
	call	r12
	cmp	rax, 34
	jne	LBB7_22
## %bb.12:                              ## %value_false10
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rax, qword ptr [r13 - 32]
	mov	qword ptr [rbx], rax
	mov	rax, qword ptr [r13 - 24]
	test	al, 1
	je	LBB7_14
## %bb.13:                              ## %fixnum_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	add	rax, 2
	jno	LBB7_15
LBB7_14:                                ## %fallback
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	qword ptr [r14 + 48], r15
	mov	qword ptr [r14 + 40], 34
	lea	rsi, [r12 + 122486656]
	mov	rdi, r14
	call	_c_push_nadd_iloc
	test	rax, rax
	je	LBB7_16
	jmp	LBB7_28
LBB7_15:                                ## %valid_true
                                        ##   in Loop: Header=BB7_1 Depth=1
	add	rbx, 16
	mov	qword ptr [r15], rax
	mov	qword ptr [r14 + 48], rbx
LBB7_16:                                ## %continue12
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rsi, qword ptr [r13 - 16]
	test	sil, 7
	jne	LBB7_24
## %bb.17:                              ## %cond1_true15
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	eax, dword ptr [rsi]
	and	eax, 15
	cmp	eax, 10
	je	LBB7_24
## %bb.18:                              ## %pair_true13
                                        ##   in Loop: Header=BB7_1 Depth=1
	mov	rcx, qword ptr [rsi + 8]
	mov	rax, qword ptr [r14 + 48]
	mov	qword ptr [rax], rcx
	mov	qword ptr [rax + 8], 3
	mov	qword ptr [rax + 16], 0
	lea	rcx, [rax + 16]
	add	rax, 24
	lea	rdx, [r12 + 122486000]
	mov	qword ptr [r14], rdx
	mov	qword ptr [r14 + 16], rax
	mov	qword ptr [r14 + 24], rcx
	mov	qword ptr [r14 + 48], rax
	jmp	LBB7_1
LBB7_19:                                ## %pair_false
	mov	qword ptr [r14 + 40], 18
	add	r12, 122485968
	jmp	LBB7_21
LBB7_20:                                ## %pair_false4
	mov	qword ptr [r14 + 48], rbx
	mov	qword ptr [r14 + 40], 34
	add	r12, 122485872
LBB7_21:                                ## %pair_false
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_car_iloc
	jmp	LBB7_28
LBB7_22:                                ## %continue
	cmp	rax, 66
	jne	LBB7_26
LBB7_23:                                ## %undef_true
	mov	qword ptr [r14 + 40], 66
	jmp	LBB7_28
LBB7_24:                                ## %pair_false14
	mov	qword ptr [r14 + 40], 34
	add	r12, 122485728
	mov	qword ptr [r14], r12
	mov	rdi, r14
	call	_c_error_push_cdr_iloc
LBB7_28:                                ## %fallback_fail
	mov	eax, 3
LBB7_29:                                ## %taken_true
	add	rsp, 24
	pop	rbx
	pop	r12
	pop	r13
	pop	r14
	pop	r15
	pop	rbp
	ret
LBB7_25:                                ## %taken_true
	mov	qword ptr [r14 + 40], 18
	jmp	LBB7_27
LBB7_26:                                ## %value_nonfalse
	mov	qword ptr [r14 + 40], 34
LBB7_27:                                ## %taken_true
	mov	eax, 1
	jmp	LBB7_29
                                        ## -- End function
	.globl	"_2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2" ## -- Begin function 2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2
	.p2align	4, 0x90
"_2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2": ## @"2caae3c4-0ae6-48d2-95c0-9b66d9c42ef2"
## %bb.0:                               ## %entry
	push	rbx
	mov	rbx, rdi
	mov	rax, qword ptr [rdi + 40]
	mov	rcx, qword ptr [rdi + 48]
	mov	qword ptr [rcx], rax
	mov	rdx, qword ptr [rdi + 16]
	movabs	rax, 4404393696
	lea	rcx, [rax + 122484112]
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
