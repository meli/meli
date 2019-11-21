#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LineBreakClass {
    BK,
    CM,
    CR,
    GL,
    LF,
    NL,
    SP,
    WJ,
    ZW,
    ZWJ,
    AI,
    AL,
    B2,
    BA,
    BB,
    CB,
    CJ,
    CL,
    CP,
    EB,
    EM,
    EX,
    H2,
    H3,
    HL,
    HY,
    ID,
    IN,
    IS,
    JL,
    JT,
    JV,
    NS,
    NU,
    OP,
    PO,
    PR,
    QU,
    RI,
    SA,
    SG,
    SY,
    XX,
}

use LineBreakClass::*;

impl From<&str> for LineBreakClass {
    fn from(val: &str) -> Self {
        match val {
            stringify!(BK) => BK,
            stringify!(CM) => CM,
            stringify!(CR) => CR,
            stringify!(GL) => GL,
            stringify!(LF) => LF,
            stringify!(NL) => NL,
            stringify!(SP) => SP,
            stringify!(WJ) => WJ,
            stringify!(ZW) => ZW,
            stringify!(ZWJ) => ZWJ,
            stringify!(AI) => AI,
            stringify!(AL) => AL,
            stringify!(B2) => B2,
            stringify!(BA) => BA,
            stringify!(BB) => BB,
            stringify!(CB) => CB,
            stringify!(CJ) => CJ,
            stringify!(CL) => CL,
            stringify!(CP) => CP,
            stringify!(EB) => EB,

            stringify!(EM) => EM,
            stringify!(EX) => EX,
            stringify!(H2) => H2,
            stringify!(H3) => H3,
            stringify!(HL) => HL,
            stringify!(HY) => HY,
            stringify!(ID) => ID,
            stringify!(IN) => IN,
            stringify!(IS) => IS,
            stringify!(JL) => JL,

            stringify!(JT) => JT,
            stringify!(JV) => JV,
            stringify!(NS) => NS,
            stringify!(NU) => NU,
            stringify!(OP) => OP,
            stringify!(PO) => PO,
            stringify!(PR) => PR,
            stringify!(QU) => QU,
            stringify!(RI) => RI,
            stringify!(SA) => SA,

            stringify!(SG) => SG,
            stringify!(SY) => SY,
            stringify!(XX) => XX,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Reflow {
    No,
    All,
    FormatFlowed,
}

impl Default for Reflow {
    fn default() -> Self {
        Reflow::FormatFlowed
    }
}
