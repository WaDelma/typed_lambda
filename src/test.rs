#[test]
fn unknown_var() {
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e = var("x");
    assert_eq!(
        Err(UnboundVar("x".into())),
        check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth)
    );
}

#[test]
fn let_id() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        let_(
            "x",
            abs(
                "y",
                var("y")
            ),
            var("x")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params }) = r {
            assert_eq!(name, "->");
            assert_eq!(
                params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
                vec![
                    Var("i2".into()),
                    Var("i2".into())
                ]
            );
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn integer() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e = var("1");
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params}) = r {
            assert_eq!(name, "I32");
            assert_eq!(params, &vec![]);
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn ret_integer() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        abs(
            "y",
            var("1")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    if let Ok(ref r) = r {
        let r = ut.probe_value(*r);
        if let App(TyFun { ref name, ref params }) = r {
            assert_eq!(name, "->");
            assert_eq!(
                params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
                vec![
                    Var("i1".into()),
                    App(TyFun::integer())
                ]
            );
        } else {
            panic!("Monotype wasn't right type function application: {:?}", r);
        }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}

#[test]
fn abs_app() {
    use Mono::*;
    let mut ctx = Ctx::new();
    let mut creator = VarCreator::new();
    let mut ut = UnificationTable::new();
    let let_depth = 128;
    let e =
        app(
            abs(
                "y",
                var("y")
            ),
            var("1")
        );
    let r = check_expr(&e, &mut ctx, &mut creator, &mut ut, let_depth);
    println!("{:?}", ctx.types);
    println!("{:#?}", ut);
    if let Ok(ref r) = r {
        println!("r: {:?}", r);
        let r = ut.probe_value(*r);
        if let Var(ref i) = r {
            assert_eq!(i, "I32");
        } else {
            panic!("Monotype wasn't variable: {:?}", r);
        }
        // if let App(TyFun { ref name, ref params}) = r {
        //     assert_eq!(name, "->");
        //     assert_eq!(
        //         params.iter().map(|p| ut.probe_value(*p)).collect::<Vec<_>>(),
        //         vec![
        //             App(TyFun::integer()),
        //             App(TyFun::integer())
        //         ]
        //     );
        // } else {
        //     if let Var(ref r) = r {
        //         println!("Var: {:?}", ctx.get(r.clone()));
        //     }
        //     panic!("Monotype wasn't right type function application: {:?}", r);
        // }
    } else {
        panic!("Checking errored: {:?}", r);
    }
}