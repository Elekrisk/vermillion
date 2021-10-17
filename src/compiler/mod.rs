use std::{borrow::Borrow, cell::RefCell, collections::{HashMap, HashSet}, iter, mem::{self}, path::Path};

use std::fmt::Debug;

use inkwell::{AddressSpace, IntPredicate, attributes::{Attribute, AttributeLoc}, basic_block::BasicBlock, builder::Builder, context::Context, memory_buffer::MemoryBuffer, module::{Linkage, Module}, types::{BasicType, BasicTypeEnum, StructType}, values::{BasicValue, BasicValueEnum, GlobalValue, IntValue, PointerValue}};
use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;

use crate::{
    lexer::token::Location,
    parser::ast::{Expression, Function, Identifier, Pattern, Program, Statement},
};

pub struct Compiler<'a> {
    program: Program,
    context: &'a Context,
    value_type: StructType<'a>,
    module: Module<'a>,
    builder: Builder<'a>,
    current_func: RefCell<Option<String>>,
    string_constants: RefCell<HashMap<String, GlobalValue<'a>>>,
    scopes: RefCell<ScopeStack<'a>>
}

type ExprRet<'a> = Result<BasicValueEnum<'a>, String>;

pub fn compile(program: Program) -> Result<(), String> {
    let context = Context::create();
    let value_type = context.struct_type(
        &[context.i64_type().into(), context.i64_type().into()],
        false,
    );

    let module = context.create_module("main");

    let builder = context.create_builder();

    {
        let compiler = Compiler {
            program,
            context: &context,
            value_type,
            module,
            builder,
            current_func: RefCell::new(None),
            string_constants: RefCell::new(HashMap::new()),
            scopes: RefCell::new(ScopeStack::default())
        };

        compiler.add_builtin_decls();
        compiler.add_function_decls()?;

        for func in &compiler.program.functions {
            compiler.compile_function(func)?;
        }

        compiler.add_real_main();

        // let buffer =
        //     MemoryBuffer::create_from_file(Path::new("./vermillion-runtime/rust/runtime.bc"))
        //         .unwrap();
        
        // let builtins = compiler.context.create_module_from_ir(buffer).unwrap();

        // compiler.module.link_in_module(builtins).map_err(|e| e.to_string())?;

        compiler.module.print_to_file("./main.ll").unwrap();
        match compiler.module.verify() {
            Ok(_) => {}
            Err(e) => return Err(e.to_string()),
        }

        // let add_func = compiler.builtin.get_function("verm!add").unwrap();
        // add_func.view_function_cfg_only();
    }

    Ok(())
}

const NIL: u8 = 0;
const BOOL: u8 = 1;
const CHAR: u8 = 2;
const FLOAT: u8 = 3;
const INT: u8 = 4;
const STRING: u8 = 5;
const TUPLE: u8 = 6;
const FUNCTION: u8 = 7;
const EXCEPTION: u8 = 255;

// ValueType:
// 0    Nil
// 1    Bool        1   i8
// 2    Char        4   i32
// 3    Float       8   f64
// 4    Int         8   ref
// 5    String      8   ref
// 6    Tuple       8   ref
// 7    Function    8   ref
// 255  Exception   8   ref

/// Adding declarations
impl<'a> Compiler<'a> {
    /// Adds all builtin functions' declarations.
    fn add_builtin_decls(&self) {
        let module = &self.module;
        let context = self.context;
        let bool_type = context.bool_type();
        let i8_type = context.i8_type();
        let i8_ptr_type = context.i8_type().ptr_type(AddressSpace::Generic);
        let i32_type = context.i8_type();
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let value_type = context.struct_type(&[i64_type.into(), i64_type.into()], false);
        let value_ptr_type = value_type.ptr_type(AddressSpace::Generic);
        let void_type = context.void_type();

        macro_rules! to_type {
            (void) => {
                void_type
            };
            (bool) => {
                bool_type
            };
            (u8) => {
                i8_type
            };
            (char) => {
                i32_type
            };
            (i64) => {
                i64_type
            };
            (u64) => {
                i64_type
            };
            (f64) => {
                f64_type
            };
            (value) => {
                value_type
            };
        }

        macro_rules! construct_parameters {
            ( $name:ident ) => {

            };
            ( $name:ident $first:ident ) => {
                $name.push(to_type!($first).into());
            };
            ( $name:ident $first:ident * ) => {
                $name.push(to_type!($first).ptr_type(AddressSpace::Generic).into());
            };
            ( $name:ident $first:ident , $($rest:tt)* ) => {
                $name.push(to_type!($first).into());
                construct_parameters! ($name $($rest)* )
            };
            ( $name:ident $first:ident *, $($rest:tt)* ) => {
                $name.push(to_type!($first).ptr_type(AddressSpace::Generic).into());
                construct_parameters! ($name $($rest)* )
            };
        }

        macro_rules! add_func_decl {
            (
                fn $name:ident ( )
            ) => {
                {
                    module.add_function(
                        stringify!($name),
                        void_type.fn_type(&[], false),
                        None
                    );
                }
            };
            (
                fn $name:ident ( ) -> $ret_type:ident
            ) => {
                {
                    module.add_function(
                        stringify!($name),
                        to_type!($ret_type).fn_type(&[], false),
                        None
                    );
                }
            };
            (
                fn $name:ident ( $($tokens:tt)* )
            ) => {
                #[allow(clippy::vec_init_then_push)]
                {
                    let mut parameters = vec![];
                    construct_parameters!( parameters $($tokens)* );
                    module.add_function(
                        stringify!($name),
                        void_type.fn_type(&parameters, false),
                        None
                    );
                }
            };
            (
                fn $name:ident ( $($tokens:tt)* ) -> $ret_type:ident
            ) => {
                #[allow(clippy::vec_init_then_push)]
                {
                    let mut parameters = vec![];
                    construct_parameters!( parameters $($tokens)* );
                    module.add_function(
                        stringify!($name),
                        to_type!($ret_type).fn_type(&parameters, false),
                        None
                    );
                }
            };
        }

        macro_rules! add_func_decls {
            {
                $(
                    fn $name:ident ( $($tokens:tt)* ) $(-> $ret_type:ident)? $(;)?
                )*
            } => {
                $(add_func_decl!(
                    fn $name ( $($tokens)* ) $(-> $ret_type)?
                );)*
            }
        }

        add_func_decls! {
            fn create_int_from_i64(i64) -> u64;
            fn create_int_from_u64(u64) -> u64;
            fn create_int_from_raw(bool, u64, u8*) -> u64;
            fn create_string_from_cstr(u8*) -> u64;
            fn create_tuple_empty() -> u64;
            fn create_tuple(u64, value*) -> u64;

            fn create_exception(value) -> u64;
            fn add_exception_frame(value, u8*, u8*, u64, u64) -> value;

            fn get_tuple_len(value*) -> u64;
            fn deconstruct_tuple(value, value*);

            fn increase_ref_count(value);
            fn drop(value);
            fn drop_in_place(value*);

            fn print(value) -> value;
            fn println(value) -> value;

            fn str(value) -> value;

            fn add(value, value) -> value;
            fn sub(value, value) -> value;
            fn mul(value, value) -> value;
            fn div(value, value) -> value;
            fn idiv(value, value) -> value;
            fn rem(value, value) -> value;
            fn eq(value, value) -> value;
            fn neq(value, value) -> value;
            fn gt(value, value) -> value;
            fn gteq(value, value) -> value;
            fn lt(value, value) -> value;
            fn lteq(value, value) -> value;
        }
    }

    /// Add all user-defined function declarations.
    /// User-defined functions are prepended with "verm!".
    /// Also checks that user-defined function names are unique.
    fn add_function_decls(&self) -> Result<(), String> {
        let module = &self.module;
        let context = self.context;
        let i64_type = context.i64_type();
        let value_type = context.struct_type(&[i64_type.into(), i64_type.into()], false);

        for function in &self.program.functions {
            let name = &function.ident.name;
            let id = format!("verm!{}", name);

            if module.get_function(&id).is_some() {
                return Err(format!("Function {name} is defined multiple times"));
            }

            let mut params = vec![];

            for param in &function.params {
                if params.contains(&param.name) {
                    return Err(format!(
                        "Function {name} has multiple parameters named {param}",
                        param = &param.name
                    ));
                }
                params.push(param.name.clone());
            }

            let param_types = iter::repeat(value_type.into())
                .take(params.len())
                .collect::<Vec<_>>();

            module.add_function(
                &id,
                value_type.fn_type(&param_types, false),
                Some(Linkage::Private),
            );
        }

        Ok(())
    }
}

/// Function compilation
impl<'a> Compiler<'a> {
    /// Compiles a function.
    fn compile_function(&self, func_def: &Function) -> Result<(), String> {
        let context = self.context;
        let module = &self.module;
        let builder = &self.builder;
        // Set current function name so that it can be used for error reporting.
        *self.current_func.borrow_mut() = Some(func_def.ident.name.clone());

        let id = format!("verm!{}", func_def.ident.name);

        // Get the function value of this function and start at the entry block.
        let func = module.get_function(&id).unwrap();
        let entry = context.append_basic_block(func, "entry");
        builder.position_at_end(entry);

        self.scopes.borrow_mut().push();

        let last = if let Some((last, slice)) = func_def.stmnts.split_last() {
            for stmnt in slice {
                // If the statement returns a value, we need to drop it to prevent leaking memory.
                let ret = self.compile_statement(stmnt)?;
                if let Some(ret) = ret {
                    self.build_drop(ret);
                }
            }

            // Return the last value.
            self.compile_statement(last)?
        } else {
            None
        };
        
        // Drop all variables declared
        for var in &self.scopes.borrow_mut().pop().unwrap().vars {
            let val = self.builder.build_load(var.alloca, &var.name);
            self.build_drop(val);
            // self.build_drop_in_place(var.alloca);
        }
        // If the last statement returns a value, return it.
        if let Some(last) = last {
            self.builder.build_return(Some(&last));
            Ok(())
        } else {
            // Else, create a Nil value and return it.
            let ret = self.build_nil();
            self.builder.build_return(Some(&ret));
            Ok(())
        }
    }
}

/// Statement compilation
impl<'a> Compiler<'a> {
    /// Compile a single statement.
    fn compile_statement(
        &self,
        stmnt: &Statement,
    ) -> Result<Option<BasicValueEnum<'a>>, String> {
        match stmnt {
            Statement::Let { .. } => self.compile_statement_let(stmnt).map(|_| None),
            Statement::Var { .. } => self.compile_statement_var(stmnt).map(|_| None),
            Statement::Break { .. } => self.compile_statement_break(stmnt).map(|_| None),
            Statement::Continue { .. } => {
                self.compile_statement_continue(stmnt).map(|_| None)
            }
            Statement::Return { .. } => self.compile_statement_return(stmnt).map(|_| None),
            Statement::Yield { .. } => self.compile_statement_yield(stmnt).map(|_| None),
            Statement::Assign { .. } => self.compile_statement_assign(stmnt).map(|_| None),
            Statement::Expression { .. } => {
                self.compile_statement_expression(stmnt).map(Some)
            }
        }
    }

    fn compile_statement_let(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Let {
            let_tok,
            pattern,
            eq,
            expr,
            semicolon,
        } = stmnt
        {
            let val = self.compile_expression(expr)?;

            let mut idents = vec![];
            self.get_pattern_identifiers(pattern, &mut idents)?;

            for ident in idents {
                let alloca = self.builder.build_alloca(self.value_type, &ident.name);

                self.scopes.borrow_mut().current.vars.push(Variable {
                    name: ident.name.clone(),
                    alloca,
                    mutable: false
                });
            }
            
            self.compile_pattern_destruction(pattern, val, true)?;

            Ok(())
        } else {
            panic!("Stmtn must be let-stmnt");
        }
    }

    fn compile_statement_var(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Var {
            var,
            pattern,
            assign,
            semicolon,
        } = stmnt
        {
            let val = if let Some((_, expr)) = assign {
                self.compile_expression(expr)?
            } else {
                self.build_nil()
            };

            let mut idents = vec![];
            self.get_pattern_identifiers(pattern, &mut idents)?;

            for ident in idents {
                let alloca = self.builder.build_alloca(self.value_type, &ident.name);

                self.scopes.borrow_mut().current.vars.push(Variable {
                    name: ident.name.clone(),
                    alloca,
                    mutable: true
                });
            }
            
            self.compile_pattern_destruction(pattern, val, true)?;

            Ok(())
        } else {
            panic!("Stmnt must be var-stmnt");
        }
    }

    fn compile_statement_break(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Break {
            break_tok,
            expr,
            semicolon,
        } = stmnt
        {
            // First, check that we are in a loop

            if !self.scopes.borrow().contains_loop() {
                return Err("Break statements cannot be used outside a loop".to_string());
            }

            // Then, evaluate the expression (if there is one) before we drop the variables

            let ret = if let Some(expr) = expr {
                self.compile_expression(expr)?
            } else {
                self.build_nil()
            };

            // Then, drop all variables which have been declared inside the loop
            // To do this we look through the top scopes, dropping all variables,
            // until we get to the scope which has a loop_info.

            let mut top = &mut *self.scopes.borrow_mut();

            while top.current.loop_info.is_none() {
                for var in &top.current.vars {
                    let val = self.builder.build_load(var.alloca, &var.name);
                    self.build_drop(val);
                }
                top = &mut **top.parent.as_mut().unwrap();
            }

            // Top should now have the block we should jump to.
            let loop_info = top.current.loop_info.as_mut().unwrap();

            // We also need to add this block and the ret value to the break_block's phi values
            loop_info.break_phi_values.push((ret, self.builder.get_insert_block().unwrap()));
            
            
            // Jump!
            self.builder.build_unconditional_branch(loop_info.break_block);

            // As a block transition must happen at the end of a block, we must create a new one and place
            // the builder at that one.
            let after = self.context.insert_basic_block_after(self.builder.get_insert_block().unwrap(), "after-break");
            self.builder.position_at_end(after);

            Ok(())
        } else {
            panic!("Stmnt must be break-stmnt");
        }
    }

    fn compile_statement_continue(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Continue {
            continue_tok,
            semicolon,
        } = stmnt
        {
            todo!()
        } else {
            panic!("Stmnt must be continue-stmnt");
        }
    }

    fn compile_statement_return(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Return {
            return_tok,
            expr,
            semicolon,
        } = stmnt
        {
            todo!()
        } else {
            panic!("Stmnt must be return-stmnt");
        }
    }

    fn compile_statement_yield(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Yield {
            yield_tok,
            expr,
            semicolon,
        } = stmnt
        {
            todo!()
        } else {
            panic!("Stmnt must be yield-stmnt");
        }
    }

    fn compile_statement_assign(
        &self,
        stmnt: &Statement,
    ) -> Result<(), String> {
        if let Statement::Assign {
            pattern,
            arrow,
            expr,
            semicolon,
        } = stmnt
        {
            let val = self.compile_expression(expr)?;
            
            self.compile_pattern_destruction(pattern, val, false)?;

            Ok(())
        } else {
            panic!("Stmnt must be assign-stmnt");
        }
    }

    fn compile_statement_expression(
        &self,
        stmnt: &Statement,
    ) -> Result<BasicValueEnum<'a>, String> {
        if let Statement::Expression { expr, semicolon } = stmnt {
            self.compile_expression(expr)
        } else {
            panic!("Stmnt must be expr-stmnt");
        }
    }
}

impl<'a> Compiler<'a> {
    fn get_pattern_identifiers<'b>(
        &self,
        pattern: &'b Pattern,
        idents: &mut Vec<&'b Identifier>,
    ) -> Result<(), String> {
        match pattern {
            Pattern::Tuple(ptrns) => {
                for ptrn in ptrns.values() {
                    self.get_pattern_identifiers(ptrn, idents)?;
                }
            }
            Pattern::Identifier(i) => {
                if idents.iter().find(|o| o.name == i.name).is_some() {
                    return Err(format!("Identifier {} exists multiple times in pattern", i.name));
                }
                idents.push(i)
            },
            _ => {}
        };
        Ok(())
    }

    fn compile_pattern_destruction(&self, pattern: &Pattern, val: BasicValueEnum<'a>, is_initializer: bool) -> Result<(), String> {
        let build_type_err = |target_type, loc| {
            let msg = self.build_string(&format!(
                "Non-{0} value cannot be destructured into {0}",
                target_type
            ));
            let err = self.build_exception(msg);
            self.build_add_exception_frame(err, loc)
        };

        match pattern {
            Pattern::Tuple(_) => todo!(),
            Pattern::Identifier(i) => {
                if !is_initializer {
                    if let Some(v) = unsafe { self.scopes.as_ptr().as_ref().unwrap() }.find_var(&i.name) {
                        if !v.mutable {
                            return Err(format!("Tried assigning to immutable variable {}", i.name));
                        } else {
                            let val = self.builder.build_load(v.alloca, &i.name);
                            self.build_drop(val);
                        }
                    }
                }
                self.build_store_variable(&i.name, val)?;
            },
            Pattern::String { contents, token } => todo!(),
            Pattern::Integer { value, token } => todo!(),
            Pattern::Float { value, token } => {
                let tag = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 0, "tag")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    tag.into_int_value(),
                    self.const_u64(FLOAT),
                    "cmp",
                );

                let isnt = self.context.insert_basic_block_after(
                    self.builder.get_insert_block().unwrap(),
                    "isnt-float",
                );
                let is = self.context.insert_basic_block_after(isnt, "is-float");
                self.builder.build_conditional_branch(cmp, is, isnt);

                self.builder.position_at_end(isnt);
                let err = build_type_err("Float", token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(is);
                let data = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 1, "data")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    data.into_int_value(),
                    self.const_u64(value.to_bits()),
                    "cmp",
                );

                let neq = self.context.insert_basic_block_after(is, "isnt-equal");
                let eq = self.context.insert_basic_block_after(neq, "is-equal");
                self.builder.build_conditional_branch(cmp, eq, neq);

                self.builder.position_at_end(neq);
                let msg = self.build_string(&format!("Deconstruction into Float {} failed", value));
                let err = self.build_exception(msg);
                let err = self.build_add_exception_frame(err, token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(eq);
                self.build_drop(val);
            },
            Pattern::Char { value, token } => {
                let tag = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 0, "tag")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    tag.into_int_value(),
                    self.const_u64(CHAR),
                    "cmp",
                );

                let isnt = self.context.insert_basic_block_after(
                    self.builder.get_insert_block().unwrap(),
                    "isnt-char",
                );
                let is = self.context.insert_basic_block_after(isnt, "is-char");
                self.builder.build_conditional_branch(cmp, is, isnt);

                self.builder.position_at_end(isnt);
                let err = build_type_err("Char", token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(is);
                let data = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 1, "data")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    data.into_int_value(),
                    self.const_u64(*value),
                    "cmp",
                );

                let neq = self.context.insert_basic_block_after(is, "isnt-equal");
                let eq = self.context.insert_basic_block_after(neq, "is-equal");
                self.builder.build_conditional_branch(cmp, eq, neq);

                self.builder.position_at_end(neq);
                let msg = self.build_string(&format!("Deconstruction into Char {} failed", value));
                let err = self.build_exception(msg);
                let err = self.build_add_exception_frame(err, token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(eq);
                self.build_drop(val);
            }
            Pattern::Bool { value, token } => {
                let tag = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 0, "tag")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    tag.into_int_value(),
                    self.const_u64(BOOL),
                    "cmp",
                );

                let isnt = self.context.insert_basic_block_after(
                    self.builder.get_insert_block().unwrap(),
                    "isnt-bool",
                );
                let is = self.context.insert_basic_block_after(isnt, "is-bool");
                self.builder.build_conditional_branch(cmp, is, isnt);

                self.builder.position_at_end(isnt);
                let err = build_type_err("Bool", token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(is);
                let data = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 1, "data")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    data.into_int_value(),
                    self.const_u64(*value),
                    "cmp",
                );

                let neq = self.context.insert_basic_block_after(is, "isnt-equal");
                let eq = self.context.insert_basic_block_after(neq, "is-equal");
                self.builder.build_conditional_branch(cmp, eq, neq);

                self.builder.position_at_end(neq);
                let msg = self.build_string(&format!("Deconstruction into Bool {} failed", value));
                let err = self.build_exception(msg);
                let err = self.build_add_exception_frame(err, token.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(eq);
                self.build_drop(val);
            }
            Pattern::Nil(tok) => {
                let tag = self
                    .builder
                    .build_extract_value(val.into_struct_value(), 0, "tag")
                    .unwrap();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    tag.into_int_value(),
                    self.const_u64(NIL),
                    "cmp",
                );

                let isnt = self
                    .context
                    .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "isnt-nil");
                let is = self.context.insert_basic_block_after(isnt, "is-nil");
                self.builder.build_conditional_branch(cmp, is, isnt);

                self.builder.position_at_end(isnt);
                let err = build_type_err("Nil", tok.loc());
                self.builder.build_return(Some(&err));

                self.builder.position_at_end(is);
                self.build_drop(val);
            }
            Pattern::Discard(_) => {}
        };

        Ok(())
    }
}

/// Expression compilation
impl<'a> Compiler<'a> {
    /// Compile a single expression
    fn compile_expression(
        &self,
        expr: &Expression,
    ) -> Result<BasicValueEnum<'a>, String> {
        let ret = match expr {
            Expression::If { .. } => self.compile_expression_if(expr)?,
            Expression::Loop(_) => self.compile_expression_loop(expr)?,
            Expression::For { .. } => self.compile_expression_for(expr)?,
            Expression::While { .. } => self.compile_expression_while(expr)?,
            Expression::Lambda { .. } => self.compile_expression_lambda(expr)?,
            Expression::Tuple(_) => self.compile_expression_tuple(expr)?,
            Expression::Binary { .. } => self.compile_expression_binary(expr)?,
            Expression::FunctionCall { .. } => {
                self.compile_expression_function_call(expr)?
            }
            Expression::MethodCall { .. } => self.compile_expression_method_call(expr)?,
            Expression::Block { .. } => self.compile_expression_block(expr)?,
            Expression::Identifier(_) => self.compile_expression_identifier(expr)?,
            Expression::String { .. } => self.compile_expression_string(expr)?,
            Expression::Integer { .. } => self.compile_expression_int(expr)?,
            Expression::Float { .. } => self.compile_expression_float(expr)?,
            Expression::Char { .. } => self.compile_expression_char(expr)?,
            Expression::Bool { .. } => self.compile_expression_bool(expr)?,
            Expression::Nil(_) => self.compile_expression_nil(expr)?,
        };

        Ok(ret)
    }

    fn compile_expression_if(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::If {
            cond,
            body,
            elifs,
            else_body,
        } = expr
        {
            let mut phi_vals = vec![];

            let final_block = self
                .context
                .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "final");

            // Generates a condition check, followed by the block, leaving the builder on the else-block
            let mut if_part = |cond: &Expression, body: &Expression| {
                // Build expression evaluation
                let c = self.compile_expression(cond)?.into_struct_value();

                // Check if expression is bool
                let t1 = self
                    .builder
                    .build_extract_value(c, 0, "tag")
                    .unwrap()
                    .into_int_value();
                let cmp = self.builder.build_int_compare(
                    IntPredicate::EQ,
                    t1,
                    self.context.i64_type().const_int(BOOL as _, false),
                    "is-cond-bool",
                );

                let cond_isnt_bool_block = self.context.insert_basic_block_after(
                    self.builder.get_insert_block().unwrap(),
                    "cond-isnt-bool",
                );
                let cond_is_bool_block = self
                    .context
                    .insert_basic_block_after(cond_isnt_bool_block, "cond-is-bool");
                self.builder.build_conditional_branch(
                    cmp,
                    cond_is_bool_block,
                    cond_isnt_bool_block,
                );

                // Condition isn't bool, return exception
                self.builder.position_at_end(cond_isnt_bool_block);
                let msg = self.build_string("Condition of If must be Bool");
                let e = self.build_exception(msg);
                // Set the location of the cond expression as the first frame information
                let e = self.build_add_exception_frame(e, &cond.loc());
                // Return instruction
                self.builder.build_return(Some(&e));

                // Condition is bool, check value
                self.builder.position_at_end(cond_is_bool_block);
                let data = self
                    .builder
                    .build_extract_value(c, 1, "data")
                    .unwrap()
                    .into_int_value();
                let cond = self
                    .builder
                    .build_int_truncate(data, self.context.bool_type(), "cond");

                let then_block = self
                    .context
                    .insert_basic_block_after(cond_is_bool_block, "then");
                let else_block = self.context.insert_basic_block_after(then_block, "else");
                self.builder
                    .build_conditional_branch(cond, then_block, else_block);

                // Generate then expression
                self.builder.position_at_end(then_block);
                let a = self.compile_expression(body)?;
                let a_block = self.builder.get_insert_block().unwrap();

                phi_vals.push((a, a_block));

                // Jump to the final block
                self.builder.build_unconditional_branch(final_block);

                // Position builder at else block for further building
                self.builder.position_at_end(else_block);

                Result::<(), String>::Ok(())
            };

            // Build first if then block
            if_part(cond, body)?;

            // For each else if, generate it's if then block
            // Due to if_part leaving the builder at the else-block,
            // if_part can be called repeatedly to form a else-if chain.
            for (cond, body) in elifs {
                if_part(cond, body)?;
            }

            // If there is an else body, build it and jump to the final block
            if let Some(else_body) = else_body {
                let ret = self.compile_expression(else_body)?;
                let block = self.builder.get_insert_block().unwrap();
                // Add the else-body's value to the phi list
                phi_vals.push((ret, block));
                self.builder.build_unconditional_branch(final_block);
            } else {
                // Build a nil expression for passing on to the final block
                // No else body is about equivalent to `else nil`
                let nil = self.build_nil();
                let block = self.builder.get_insert_block().unwrap();
                phi_vals.push((nil, block));
                self.builder.build_unconditional_branch(final_block);
            }

            // The final block is comprised with a phi node,
            self.builder.position_at_end(final_block);
            let phi = self.builder.build_phi(self.value_type, "phi");
            for (val, block) in phi_vals {
                phi.add_incoming(&[(&val, block)]);
            }

            // Leaving the builder on the final block for further expressions.
            Ok(phi.as_basic_value())
        } else {
            panic!("Expr must be if-expr");
        }
    }

    fn compile_expression_loop(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Loop(body) = expr {
            // The block to loop back to.
            let loop_block = self
                .context
                .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "loop");
            // The loop exit block is created early so it can be added
            // to `scopes`, so that statements inside the loop can jump to it
            // (i.e break statements).
            let break_block = self
                .context
                .insert_basic_block_after(loop_block, "loop-exit");

            // We need to enter the block
            self.builder.build_unconditional_branch(loop_block);
            self.builder.position_at_end(loop_block);

            self.scopes.borrow_mut().push();
            // As a loop expression itself cannot introduce variables, there is no need for a separate continue block,
            // and a continue statement can instead jump directly to the loop start.
            // This is assuming that the continue statement drops any values allocated in the scopes above this one,
            // as it is supposed to do.
            self.scopes.borrow_mut().current.loop_info = Some(LoopInfo {
                continue_block: loop_block,
                break_block,
                break_phi_values: vec![],
            });

            let ret = self.compile_expression(body)?;
            // Drop the returned value
            self.build_drop(ret);

            self.builder.build_unconditional_branch(loop_block);

            // We need to create a phi instruction, as a break statement can make the loop evaluate to something other than nil.
            // However, if there is no break statement (an infinite loop), a phi node shouldn't be created,
            // as a phi node needs at least one predecessor.
            self.builder.position_at_end(break_block);
            let scope_info = self.scopes.borrow_mut().pop().unwrap();
            let loop_info = scope_info.loop_info.unwrap();
            if !loop_info.break_phi_values.is_empty() {
                let phi = self.builder.build_phi(self.value_type, "loop-ret");
                for (val, block) in loop_info.break_phi_values {
                    phi.add_incoming(&[(&val, block)]);
                }

                Ok(phi.as_basic_value())
            } else {
                // Due to how I've setup the compiling functions, this function must return a value, even if it is an infinite loop.
                // However, during optimization this should be entirely erased.
                Ok(self.build_nil())
            }
        } else {
            panic!("Expr must be loop-expr");
        }
    }

    fn compile_expression_for(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::For {
            pattern,
            expr,
            body,
        } = expr
        {
            todo!()
        } else {
            panic!("Expr must be for-expr");
        }
    }

    fn compile_expression_while(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::While { cond, body } = expr {
            todo!()
        } else {
            panic!("Expr must be while-expr");
        }
    }

    fn compile_expression_lambda(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Lambda {
            params,
            arrow,
            body,
        } = expr
        {
            todo!()
        } else {
            panic!("Expr must be lambda-expr");
        }
    }

    fn compile_expression_tuple(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Tuple(values) = expr {
            todo!()
        } else {
            panic!("Expr must be tuple-expr");
        }
    }

    fn compile_expression_binary(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Binary { left, right, op } = expr {
            let l = self.compile_expression(left)?;
            let r = self.compile_expression(right)?;

            let func_name = match op.op {
                crate::parser::ast::BinaryOperatorType::And => todo!(),
                crate::parser::ast::BinaryOperatorType::Or => todo!(),
                crate::parser::ast::BinaryOperatorType::Eq => "eq",
                crate::parser::ast::BinaryOperatorType::Neq => "neq",
                crate::parser::ast::BinaryOperatorType::Gt => "gt",
                crate::parser::ast::BinaryOperatorType::Gteq => "gteq",
                crate::parser::ast::BinaryOperatorType::Lt => "lt",
                crate::parser::ast::BinaryOperatorType::Lteq => "lteq",
                crate::parser::ast::BinaryOperatorType::Plus => "add",
                crate::parser::ast::BinaryOperatorType::Minus => "sub",
                crate::parser::ast::BinaryOperatorType::Multiply => "mul",
                crate::parser::ast::BinaryOperatorType::Divide => "div",
                crate::parser::ast::BinaryOperatorType::IntegerDivide => "idiv",
                crate::parser::ast::BinaryOperatorType::Remainder => "rem",
            };
            Ok(self.build_checked_call(func_name, &[l, r], op.token.loc()))
        } else {
            panic!("Expr must be binary-expr");
        }
    }

    fn compile_expression_function_call(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::FunctionCall {
            func,
            excl_mark,
            args,
        } = expr
        {
            todo!()
        } else {
            panic!("Expr must be funccall-expr");
        }
    }

    fn compile_expression_method_call(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::MethodCall {
            object,
            method_name,
            args,
        } = expr
        {
            todo!()
        } else {
            panic!("Expr must be methodcall-expr");
        }
    }

    fn compile_expression_block(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Block {
            lbrace,
            statements,
            rbrace,
        } = expr
        {
            self.scopes.borrow_mut().push();

            let last = if let Some((last, slice)) = statements.split_last() {
                for stmnt in slice {
                    // If the statement returns a value, we need to drop it to prevent leaking memory.
                    let ret = self.compile_statement(stmnt)?;
                    if let Some(ret) = ret {
                        self.build_drop(ret);
                    }
                }

                self.compile_statement(last)?
            } else {
                None
            };
            
            // Drop all variables declared
            for var in &self.scopes.borrow_mut().pop().unwrap().vars {
                let val = self.builder.build_load(var.alloca, &var.name);
                self.build_drop(val);
                // self.build_drop_in_place(var.alloca);
            }
            
            if let Some(last) = last {
                Ok(last)
            } else {
                let ret = self.build_nil();
                Ok(ret)
            }
        } else {
            panic!("Expr must be block-expr");
        }
    }

    fn compile_expression_identifier(
        &self,
        expr: &Expression,
    ) -> ExprRet<'a> {
        if let Expression::Identifier(ident) = expr {
            self.build_load_variable(&ident.name)
        } else {
            panic!("Expr must be tuple-expr");
        }
    }

    fn compile_expression_string(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::String { contents, .. } = expr {
            Ok(self.build_string(contents))
        } else {
            panic!("Expr must be string-expr");
        }
    }

    fn compile_expression_int(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::Integer { value, .. } = expr {
            Ok(self.build_int(value))
        } else {
            panic!("Expr must be int-expr");
        }
    }

    fn compile_expression_float(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::Float { value, .. } = expr {
            Ok(self.build_float(*value))
        } else {
            panic!("Expr must be float-expr");
        }
    }

    fn compile_expression_char(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::Char { value, .. } = expr {
            Ok(self.build_char(*value))
        } else {
            panic!("Expr must be char-expr");
        }
    }

    fn compile_expression_bool(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::Bool { value, .. } = expr {
            Ok(self.build_bool(*value))
        } else {
            panic!("Expr must be bool-expr");
        }
    }

    fn compile_expression_nil(&self, expr: &Expression) -> ExprRet<'a> {
        if let Expression::Nil(_) = expr {
            Ok(self.build_nil())
        } else {
            panic!("Expr must be nil-expr");
        }
    }
}

/// Utilities compilation
impl<'a> Compiler<'a> {
    /// Builds a call to the builtin `drop`.
    fn build_drop(&self, value: BasicValueEnum<'a>) {
        // We check if the value actually needs dropping, as the call to
        // `drop` cannot be optimized out otherwise, as it is an external function.

        let tag = self
            .builder
            .build_extract_value(value.into_struct_value(), 0, "tag")
            .unwrap();
        let cmp = self.builder.build_int_compare(
            IntPredicate::UGT,
            tag.into_int_value(),
            self.const_u64(FLOAT),
            "cmp",
        );

        let needs_dropping = self
            .context
            .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "needs-dropping");
        let after_drop = self
            .context
            .insert_basic_block_after(needs_dropping, "after-drop");

        self.builder
            .build_conditional_branch(cmp, needs_dropping, after_drop);

        self.builder.position_at_end(needs_dropping);

        let drop = self.module.get_function("drop").unwrap();
        self.builder.build_call(drop, &[value], "");
        self.builder.build_unconditional_branch(after_drop);

        self.builder.position_at_end(after_drop);
    }

    /// Builds a call to the builtin `drop_in_place`.
    fn build_drop_in_place(&self, value: PointerValue<'a>) {
        // We check if the value actually needs dropping, as the call to
        // `drop` cannot be optimized out otherwise, as it is an external function.

        let tag_ptr = unsafe {
            self
            .builder
            .build_in_bounds_gep(value, &[self.const_u32(0), self.const_u32(0)], "tag")
        };
        let tag = self.builder.build_load(tag_ptr, "tag");
        let cmp = self.builder.build_int_compare(
            IntPredicate::UGT,
            tag.into_int_value(),
            self.const_u64(FLOAT),
            "cmp",
        );

        let needs_dropping = self
            .context
            .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "needs-dropping");
        let after_drop = self
            .context
            .insert_basic_block_after(needs_dropping, "after-drop");

        self.builder
            .build_conditional_branch(cmp, needs_dropping, after_drop);

        self.builder.position_at_end(needs_dropping);

        let drop = self.module.get_function("drop_in_place").unwrap();
        self.builder.build_call(drop, &[value.into()], "");
        self.builder.build_unconditional_branch(after_drop);

        self.builder.position_at_end(after_drop);
    }

    /// Builds a call to the builtin `increase_ref_count`.
    fn build_increase_ref_count(&self, value: BasicValueEnum<'a>) {
        // We check if the value actually needs refcounting, as the call to
        // `increase_ref_count` cannot be optimized out otherwise, as it is an external function.

        let tag = self
            .builder
            .build_extract_value(value.into_struct_value(), 0, "tag")
            .unwrap();
        let cmp = self.builder.build_int_compare(
            IntPredicate::UGT,
            tag.into_int_value(),
            self.const_u64(FLOAT),
            "cmp",
        );

        let needs_refcounting = self
            .context
            .insert_basic_block_after(self.builder.get_insert_block().unwrap(), "needs-refcounting");
        let after_refcount = self
            .context
            .insert_basic_block_after(needs_refcounting, "after-refcount");

        self.builder
            .build_conditional_branch(cmp, needs_refcounting, after_refcount);

        self.builder.position_at_end(needs_refcounting);

        let drop = self.module.get_function("increase_ref_count").unwrap();
        self.builder.build_call(drop, &[value], "");
        self.builder.build_unconditional_branch(after_refcount);

        self.builder.position_at_end(after_refcount);
    }

    /// Builds a call that doesn't check for exceptions.
    /// This is a pretty thin wrapper around `builder.build_call`.
    /// This shouldn't be used with functions that return a Value.
    fn build_unchecked_call(&self, func: &str, args: &[BasicValueEnum<'a>]) -> Option<BasicValueEnum<'a>> {
        eprintln!("Building unchecked call to {}", func);
        let func_val = self.module.get_function(func).unwrap();
        self.builder
            .build_call(func_val, args, &format!("{func}-ret"))
            .try_as_basic_value()
            .left()
    }

    /// Builds a call that checks if the result is an exception,
    /// and if so, adds the current position to the stack trace
    /// and returns it.
    fn build_checked_call(
        &self,
        func: &str,
        args: &[BasicValueEnum<'a>],
        loc: &Location,
    ) -> BasicValueEnum<'a> {
        eprintln!("Building checked call to {}", func);
        let func_val = self.module.get_function(func).unwrap();
        let ret = self
            .builder
            .build_call(func_val, args, &format!("{func}-ret"))
            .try_as_basic_value()
            .unwrap_left();
        let tag = self
            .builder
            .build_extract_value(ret.into_struct_value(), 0, "tag")
            .unwrap();
        let cmp = self.builder.build_int_compare(
            IntPredicate::EQ,
            tag.into_int_value(),
            self.const_u64(EXCEPTION),
            "cmp",
        );

        let current_block = self.builder.get_insert_block().unwrap();
        let exception_block = self
            .context
            .insert_basic_block_after(current_block, "exception");
        let continue_block = self
            .context
            .insert_basic_block_after(exception_block, "continue");
        self.builder
            .build_conditional_branch(cmp, exception_block, continue_block);

        self.builder.position_at_end(exception_block);
        let file_name = self.global_as_str(self.build_const_str(loc.file()));
        let func_name = self.global_as_str(
            self.build_const_str(
                self.current_func
                    .borrow()
                    .as_ref()
                    .map(|v| v.as_str())
                    .unwrap_or("???"),
            ),
        );
        let line = self.const_u64(loc.start().row());
        let col = self.const_u64(loc.start().col());
        let eret = self
            .builder
            .build_call(
                self.module.get_function("add_exception_frame").unwrap(),
                &[
                    ret,
                    file_name.into(),
                    func_name.into(),
                    line.into(),
                    col.into(),
                ],
                "ret",
            )
            .try_as_basic_value()
            .unwrap_left();
        self.builder.build_return(Some(&eret));

        self.builder.position_at_end(continue_block);
        ret
    }

    /// Builds a Nil construction without calling to the builtin
    fn build_nil(&self) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "nil-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder
            .build_store(tag_ptr, self.context.i64_type().const_int(NIL as _, false));
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };
        self.builder.build_store(data_ptr, self.const_u64(0 as u64));
        self.builder.build_load(alloca, "nil")
    }

    /// Builds a Bool construction without calling to the builtin
    fn build_bool(&self, val: bool) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "bool-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder
            .build_store(tag_ptr, self.context.i64_type().const_int(BOOL as _, false));
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };
        self.builder
            .build_store(data_ptr, self.const_u64(val as u64));
        self.builder.build_load(alloca, "bool")
    }

    fn build_char(&self, val: char) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "char-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder
            .build_store(tag_ptr, self.context.i64_type().const_int(CHAR as _, false));
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };
        self.builder
            .build_store(data_ptr, self.const_u64(val as u64));
        self.builder.build_load(alloca, "char")
    }

    fn build_float(&self, val: f64) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "float-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder.build_store(
            tag_ptr,
            self.context.i64_type().const_int(FLOAT as _, false),
        );
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };
        self.builder
            .build_store(data_ptr, self.const_u64(val.to_bits()));
        self.builder.build_load(alloca, "float")
    }

    fn build_int(&self, val: &BigInt) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "int-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder
            .build_store(tag_ptr, self.context.i64_type().const_int(INT as _, false));
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };

        let data = if let Some(val) = val.to_u64() {
            self.build_unchecked_call("create_int_from_u64", &[self.const_u64(val).into()]).unwrap()
        } else if let Some(val) = val.to_i64() {
            self.build_unchecked_call("create_int_from_i64", &[self.const_u64(val as u64).into()]).unwrap()
        } else {
            let (sign, bytes) = val.to_bytes_le();

            let negative = sign == Sign::Minus;

            let typ = self.context.i8_type().array_type(bytes.len() as _);

            let global = self.module.add_global(typ, None, "const.bigint.bytes");
            global.set_constant(true);
            global.set_unnamed_addr(true);
            let bytes = bytes
                .into_iter()
                .map(|b| self.const_u8(b))
                .collect::<Vec<_>>();
            global.set_initializer(&self.context.i8_type().const_array(bytes.as_slice()));
            let ptr = self.global_as_str(global);
            self.build_unchecked_call(
                "create_int_from_raw",
                &[
                    self.const_bool(negative).into(),
                    self.const_u64(bytes.len()).into(),
                    ptr.into(),
                ],
            ).unwrap()
        };

        self.builder.build_store(data_ptr, data);
        self.builder.build_load(alloca, "int")
    }

    fn build_string(&self, val: &str) -> BasicValueEnum<'a> {
        let alloca = self.builder.build_alloca(self.value_type, "string-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder.build_store(
            tag_ptr,
            self.context.i64_type().const_int(STRING as _, false),
        );
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };

        let ptr = self.global_as_str(self.build_const_str(val));
        let data = self.build_unchecked_call("create_string_from_cstr", &[ptr.into()]).unwrap();

        self.builder.build_store(data_ptr, data);
        self.builder.build_load(alloca, "string")
    }

    fn build_exception(&self, val: BasicValueEnum<'a>) -> BasicValueEnum<'a> {
        let alloca = self
            .builder
            .build_alloca(self.value_type, "exception-alloca");
        let tag_ptr = unsafe {
            self.builder.build_gep(
                alloca,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(0, false),
                ],
                "tag-ptr",
            )
        };
        self.builder.build_store(
            tag_ptr,
            self.context.i64_type().const_int(EXCEPTION as _, false),
        );
        let data_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                alloca,
                &[self.const_u32(0), self.const_u32(1)],
                "data-ptr",
            )
        };

        let data = self.build_unchecked_call("create_exception", &[val]).unwrap();

        self.builder.build_store(data_ptr, data);
        self.builder.build_load(alloca, "exception")
    }

    fn build_add_exception_frame(
        &self,
        err: BasicValueEnum<'a>,
        loc: &Location,
    ) -> BasicValueEnum<'a> {
        let file_name = self.build_const_str(loc.file());
        let func_name = self.build_const_str(
            self.current_func
                .borrow()
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or("???"),
        );
        let line = loc.start().row();
        let col = loc.start().col();
        self.builder
            .build_call(
                self.module.get_function("add_exception_frame").unwrap(),
                &[
                    err,
                    self.global_as_str(file_name).into(),
                    self.global_as_str(func_name).into(),
                    self.context.i64_type().const_int(line as _, false).into(),
                    self.context.i64_type().const_int(col as _, false).into(),
                ],
                "exception",
            )
            .try_as_basic_value()
            .unwrap_left()
    }

    fn const_bool<T: TryInto<u64>>(&self, v: T) -> IntValue<'a>
    where
        T::Error: Debug,
    {
        self.context
            .bool_type()
            .const_int(v.try_into().unwrap(), false)
    }

    fn const_u8<T: TryInto<u64>>(&self, v: T) -> IntValue<'a>
    where
        T::Error: Debug,
    {
        self.context
            .i8_type()
            .const_int(v.try_into().unwrap(), false)
    }

    fn const_u32<T: TryInto<u64>>(&self, v: T) -> IntValue<'a>
    where
        T::Error: Debug,
    {
        self.context
            .i32_type()
            .const_int(v.try_into().unwrap(), false)
    }

    fn const_u64<T: TryInto<u64>>(&self, v: T) -> IntValue<'a>
    where
        T::Error: Debug,
    {
        self.context
            .i64_type()
            .const_int(v.try_into().unwrap(), false)
    }

    fn build_const_str(&self, string: &str) -> GlobalValue<'a> {
        if let Some(val) = self.string_constants.borrow().get(string) {
            return *val;
        }

        let global = self.module.add_global(
            self.context.i8_type().array_type(string.len() as u32 + 1),
            None,
            "const.str",
        );
        global.set_constant(true);
        global.set_unnamed_addr(true);
        global.set_linkage(Linkage::Private);
        global.set_initializer(
            &self.context.i8_type().const_array(
                string
                    .bytes()
                    .chain(iter::once(0))
                    .map(|b| self.context.i8_type().const_int(b as _, false))
                    .collect::<Vec<_>>()
                    .as_slice(),
            ),
        );

        self.string_constants
            .borrow_mut()
            .insert(string.to_string(), global);

        global
    }

    fn global_as_str(&self, global: GlobalValue<'a>) -> PointerValue<'a> {
        unsafe {
            global
                .as_pointer_value()
                .const_in_bounds_gep(&[self.const_u32(0), self.const_u32(0)])
        }
    }

    fn build_load_variable(&self, name: &str) -> Result<BasicValueEnum<'a>, String> {
        let var = unsafe { self.scopes.as_ptr().as_ref() }.unwrap().find_var(name).ok_or_else(|| format!("Variable {} not found", name))?;
        let ptr = var.alloca;
        let val = self.builder.build_load(ptr, &var.name.to_string());
        self.build_increase_ref_count(val);
        Ok(val)
    }

    fn build_store_variable(&self, name: &str, val: BasicValueEnum<'a>) -> Result<(), String> {
        let scopes = self.scopes.borrow();
        let var = scopes.find_var(name).ok_or_else(|| format!("Variable {} not found", name))?;
        let ptr = var.alloca;
        self.builder.build_store(ptr, val);

        Ok(())
    }
}

/// Other
impl<'a> Compiler<'a> {
    fn add_real_main(&self) {
        let main =
            self.module
                .add_function("main", self.context.i32_type().fn_type(&[], false), None);
        let entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(entry);

        let main = self.module.get_function("verm!main").unwrap();
        let ret = self
            .builder
            .build_call(main, &[], "ret")
            .try_as_basic_value()
            .unwrap_left();
        self.builder
            .build_call(self.module.get_function("println").unwrap(), &[ret], "");
        self.builder.build_return(Some(&self.const_u32(0)));
    }
}

#[derive(Clone)]
struct Variable<'a> {
    name: String,
    alloca: PointerValue<'a>,
    mutable: bool
}

type ScopeStack<'a> = Stack<ScopeInfo<'a>>;

#[derive(Default)]
struct ScopeInfo<'a> {
    vars: Vec<Variable<'a>>,
    loop_info: Option<LoopInfo<'a>>,
}

struct LoopInfo<'a> {
    continue_block: BasicBlock<'a>,
    break_block: BasicBlock<'a>,
    break_phi_values: Vec<(BasicValueEnum<'a>, BasicBlock<'a>)>,
}

struct Stack<T> {
    parent: Option<Box<Stack<T>>>,
    current: T,
}

impl<T> Stack<T> {
    fn new(val: T) -> Self {
        Self {
            parent: None,
            current: val,
        }
    }

    fn push_with(&mut self, val: T) {
        let mut parent = Stack::new(val);
        mem::swap(self, &mut parent);
        self.parent = Some(Box::new(parent));
    }

    fn pop(&mut self) -> Option<T> {
        self.parent.take().map(|mut parent| {
            mem::swap(self, &mut parent);
            parent.current
        })
    }
}

impl<T: Default> Stack<T> {
    fn push(&mut self) {
        let mut parent = Stack::new(T::default());
        mem::swap(self, &mut parent);
        self.parent = Some(Box::new(parent));
    }
}

impl<T: Default> Default for Stack<T> {
    fn default() -> Self {
        Self {
            parent: None,
            current: T::default(),
        }
    }
}

impl<T> Stack<Vec<T>> {
    fn add(&mut self, val: T) {
        self.current.push(val);
    }

    fn find<F: FnMut(&&T) -> bool + Copy>(&self, f: F) -> Option<&T> {
        self.current
            .iter()
            .find(f)
            .or_else(|| self.parent.as_ref().map(|p| p.find(f)).flatten())
    }
}

impl<'a> ScopeStack<'a> {
    fn find_var(&self, name: &str) -> Option<&Variable> {
        self.current
            .vars
            .iter()
            .rev()
            .find(|v| v.name == name)
            .or_else(|| self.parent.as_ref().map(|p| p.find_var(name)).flatten())
    }

    fn contains_loop(&self) -> bool {
        self.current.loop_info.is_some()
            || self
                .parent
                .as_ref()
                .map(|p| p.contains_loop())
                .unwrap_or(false)
    }
}
