pub struct FieldInfo<'a> {
    pub(crate) idents: Vec<&'a Option<syn::Ident>>,
    pub(crate) types: Vec<WrapType<'a>>,
    pub(crate) attrs: Vec<&'a Vec<syn::Attribute>>,
}
pub struct WrapType<'a> {
    /// origin syn Type
    pub(crate) ty: &'a syn::Type,

    /// inner Type
    /// may be something we don't care
    pub(crate) inner: Option<&'a syn::Type>,

    /// origin syn Type name
    /// for now we do care Option & Vec
    pub(crate) tyn: TypeName,
}

impl<'a> WrapType<'a> {
    pub(crate) fn new(ty: &'a syn::Type, inner: Option<&'a syn::Type>, tyn: TypeName) -> Self {
        WrapType { ty, inner, tyn }
    }
}

#[derive(PartialEq, Eq)]
pub enum TypeName {
    Option,
    Vector,
    Whocares,
}
