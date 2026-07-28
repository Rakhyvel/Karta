use crate::{
    ast::{Ast, AstId},
    KartaContext,
};

#[derive(Clone)]
/// A struct representing a query over a Karta expression and its intermediate result. This allows the user to
/// efficiently manipulate a result of a Karta computation.
pub struct KartaQuery<'a> {
    /// The Karta context that this query is over
    context: &'a KartaContext,
    /// The current, immediate result of this query.
    current_result: Result<AstId, String>,
}

impl<'a> KartaQuery<'a> {
    /// Create a new query within a Karta context, with a given working result.
    pub(crate) fn new(context: &'a KartaContext, current_result: AstId) -> Self {
        Self {
            context,
            current_result: Ok(current_result),
        }
    }

    /// Interpret the current result of this query as an integer.
    /// Returns an error if the query result cannot be converted to an integer, or if any errors occured during the
    /// query process.
    pub fn as_int<T>(&self) -> Result<T, String>
    where
        T: From<i64>,
    {
        let ast_heap = self.context.ast_heap();

        if let Ok(current_result) = self.current_result {
            let ast = ast_heap
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x)),
                Ast::Float(x) => Ok(T::from(*x as i64)),
                Ast::Char(x) => Ok(T::from(*x as i64)),
                _ => Err(format!("cannot convert {:?} to int", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Interpret the current result of this query as a float.
    /// Returns an error if the query result cannot be converted to a float, or if any errors occured during the query process.
    pub fn as_float<T>(&self) -> Result<T, String>
    where
        T: From<f64>,
    {
        let ast_heap = self.context.ast_heap();

        if let Ok(current_result) = self.current_result {
            let ast = ast_heap
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x as f64)),
                Ast::Float(x) => Ok(T::from(*x)),
                _ => Err(format!("cannot convert {:?} to float", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Interpret the current result of this query as a string.
    /// Returns an error if the query result cannot be converted to a string, or if any errors occured during the query process.
    pub fn as_string(&self) -> Result<String, String> {
        let ast_heap = self.context.ast_heap();
        let strings = self.context.strings();

        if let Ok(current_result) = self.current_result {
            let ast = ast_heap
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::String(x) => Ok(strings.get(*x).to_owned()),
                _ => Err(format!("cannot convert {:?} to string", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Whether or not the current result of this query is truthy (ie not the .nil atom).
    /// Propagates any errors that may have occured during the query process.
    pub fn truthy(&self) -> Result<bool, String> {
        let ast_heap = self.context.ast_heap();

        if let Ok(current_result) = self.current_result {
            let ast = ast_heap
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Atom(_x) => todo!("figure out how to do truthy atoms"),
                _ => Ok(true),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Whether or not the current result of this query is truthy (ie the .nil atom).
    /// Propagates any errors that may have occured during the query process.
    pub fn falsey(&self) -> Result<bool, String> {
        Ok(!(self.truthy()?))
    }

    fn is_empty_map(&self) -> Result<bool, String> {
        let ast_heap = self.context.ast_heap();

        if let Ok(current_result) = self.current_result {
            let ast = ast_heap
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Map(x) => Ok(x.is_empty()),
                _ => Ok(false),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }
}

impl<'a> IntoIterator for KartaQuery<'a> {
    type Item = KartaQuery<'a>;

    type IntoIter = KartaListIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        KartaListIterator { query: self }
    }
}

pub struct KartaListIterator<'a> {
    query: KartaQuery<'a>,
}

impl<'a> Iterator for KartaListIterator<'a> {
    type Item = KartaQuery<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.query.is_empty_map().unwrap() {
            None
        } else {
            todo!("figure out how to do iteration")
        }
    }
}
