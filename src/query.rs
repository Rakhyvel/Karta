use crate::{
    ast::{Ast, AstId},
    KartaFile,
};

#[derive(Clone)]
/// A struct representing a query over a Karta file and it's intermediate result
pub struct KartaQuery<'a> {
    /// The Karta file that this query is over
    file: &'a KartaFile,
    /// The current, immediate result of this query.
    current_result: Result<AstId, String>,
}

impl<'a> KartaQuery<'a> {
    /// Create a new query with a Karta file and current result set as the file's root.
    pub(crate) fn new(file: &'a KartaFile) -> Self {
        Self {
            file,
            current_result: Ok(file.root),
        }
    }

    /// Return a new query with it's result being the result of applying the atom to the current result.
    /// The result becomes an error if applied to a non-map, or if the previous result was errant.
    pub fn get_atom(mut self, field: &str) -> Self {
        let current_result = match self.current_result {
            Ok(x) => x,
            Err(_) => return self,
        };

        let root_ast = self
            .file
            .ast_heap()
            .get(current_result)
            .expect("couldn't get Ast for AstId");

        let field_atom_id = match self.file.atoms().get(field) {
            Some(x) => x,
            None => {
                self.current_result = Ok(AstId::new(0));
                return self;
            }
        };

        self.current_result = match root_ast {
            Ast::Map(map) => Ok(*map.get(&field_atom_id).expect("unreachable code")),
            _ => Err(format!("cannot call `get` on {:?} type AST", root_ast)),
        };

        self
    }

    /// Interpret the current result of this query as an integer.
    /// Returns an error if the query result cannot be converted to an integer, or if any errors occured during the query process.
    pub fn as_int<T>(&self) -> Result<T, String>
    where
        T: From<i64>,
    {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x as i64)),
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
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Int(x) => Ok(T::from(*x as f64)),
                Ast::Float(x) => Ok(T::from(*x as f64)),
                Ast::Char(x) => Ok(T::from(*x as f64)),
                _ => Err(format!("cannot convert {:?} to float", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Interpret the current result of this query as a string.
    /// Returns an error if the query result cannot be converted to a string, or if any errors occured during the query process.
    pub fn as_string(&self) -> Result<&str, String> {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::String(x) => Ok((*x).as_str()),
                _ => Err(format!("cannot convert {:?} to string", ast)),
            }
        } else {
            Err(self.current_result.clone().unwrap_err())
        }
    }

    /// Whether or not the current result of this query is truthy (ie not the .nil atom).
    /// Propagates any errors that may have occured during the query process.
    pub fn truthy(&self) -> Result<bool, String> {
        if let Ok(current_result) = self.current_result {
            let ast = self
                .file
                .ast_heap()
                .get(current_result)
                .expect("couldn't get Ast for AstId");
            match ast {
                Ast::Atom(x) => Ok(x.as_usize() != 0),
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

    // TODO: Implement IntoIterator for lists
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
        let retval = self.query.clone().get_atom(".head");

        let tail = self.query.clone().get_atom(".tail");
        if tail.truthy().unwrap_or(false) {
            self.query = tail.clone();
            Some(retval)
        } else {
            None
        }
    }
}
