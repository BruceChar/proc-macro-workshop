use proc_macro2::Ident;

pub struct FieldInfo<'a> {
    pub(crate) idents: Vec<&'a Option<syn::Ident>>,
    pub(crate) types: Vec<WrapType<'a>>,
    // pub(crate) attrs: Vec<&'a Vec<syn::Attribute>>,
    pub(crate) attrs: Vec<WrapAttr<'a>>,
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

#[derive(Debug, PartialEq, Eq)]
pub enum TypeName {
    Option,
    Vector,
    Whocares,
}
#[allow(dead_code)]
pub struct WrapAttr<'a> {
    pub(crate) path: Option<&'a Ident>,
    pub(crate) meta: MetaType<'a>,
    pub(crate) ty: AttrType,
}

impl<'a> WrapAttr<'a> {
    pub fn new(path: Option<&'a Ident>, meta: MetaType<'a>, ty: AttrType) -> Self {
        WrapAttr { path, meta, ty }
    }

    pub fn default() -> Self {
        WrapAttr {
            path: None,
            meta: MetaType::None,
            ty: AttrType::None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum MetaType<'a> {
    Path(Ident),
    List(Vec<MetaType<'a>>),
    NameValue(&'a str, Ident),
    None,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AttrType {
    Each,
    None,
}
