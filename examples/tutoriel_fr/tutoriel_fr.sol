// This file has been generated by the Catala compiler, do not edit!

pragma solidity ^0.8.19;

type Unit is bool;

struct Duration { int256 value; }
function eq(Duration calldata a, Duration calldata b) pure returns (bool) { return(a.value == b.value); }
function ne(Duration calldata a, Duration calldata b) pure returns (bool) { return(!eq(a, b)); }

struct Money { Integer value; }
function eq(Money calldata a, Money calldata b) pure returns (bool) { return(eq(a.value, b.value)); }
function ne(Money calldata a, Money calldata b) pure returns (bool) { return(!eq(a, b)); }

struct Integer { int256 value; }
function eq(Integer calldata a, Integer calldata b) pure returns (bool) { return(a.value == b.value); }
function ne(Integer calldata a, Integer calldata b) pure returns (bool) { return(!eq(a, b)); }

struct Decimal { fixed256x80 value; }
function eq(Decimal calldata a, Decimal calldata b) pure returns (bool) { return(a.value == b.value); }
function ne(Decimal calldata a, Decimal calldata b) pure returns (bool) { return(!eq(a, b)); }

struct Date { int64 value; }
function eq(Date calldata a, Date calldata b) pure returns (bool) { return(a.value == b.value); }
function ne(Date calldata a, Date calldata b) pure returns (bool) { return(!eq(a, b)); }

enum CreditImpot {
    AucunCreditImpot,
    CreditImpotEnfants
}

struct Personne {
    Money revenu;
    Integer nombre_enfants;
}

function eq(Personne calldata a, Personne calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.revenu, b.revenu) && 
        eq(a.nombre_enfants, b.nombre_enfants)
    );
}

function eq(Personne[] calldata a, Personne[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct CalculImpotRevenu {
    Money impot_revenu;
}

function eq(CalculImpotRevenu calldata a, CalculImpotRevenu calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(CalculImpotRevenu[] calldata a, CalculImpotRevenu[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct DeuxTranches {
    Money seuil;
    Decimal taux1;
    Decimal taux2;
}

function eq(DeuxTranches calldata a, DeuxTranches calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.seuil, b.seuil) && 
        eq(a.taux1, b.taux1) && 
        eq(a.taux2, b.taux2)
    );
}

function eq(DeuxTranches[] calldata a, DeuxTranches[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct CalculImpotDeuxTranches {
    function (Money calldata) returns (Money calldata) formule_imposition;
}

function eq(CalculImpotDeuxTranches calldata a, CalculImpotDeuxTranches calldata b)
    pure
    returns (bool)
{
    return(
        (a.formule_imposition == b.formule_imposition)
    );
}

function eq(CalculImpotDeuxTranches[] calldata a, CalculImpotDeuxTranches[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct NouveauCalculImpotRevenu {
    Money impot_revenu;
}

function eq(NouveauCalculImpotRevenu calldata a, NouveauCalculImpotRevenu calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(NouveauCalculImpotRevenu[] calldata a, NouveauCalculImpotRevenu[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct Test1 {
    Money impot_revenu;
}

function eq(Test1 calldata a, Test1 calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(Test1[] calldata a, Test1[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct Test2 {
    Money impot_revenu;
}

function eq(Test2 calldata a, Test2 calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(Test2[] calldata a, Test2[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct NouveauCalculImpotRevenuCorrect {
    Money impot_revenu;
}

function eq(NouveauCalculImpotRevenuCorrect calldata a, NouveauCalculImpotRevenuCorrect calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(NouveauCalculImpotRevenuCorrect[] calldata a, NouveauCalculImpotRevenuCorrect[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct Test3 {
    Money impot_revenu;
}

function eq(Test3 calldata a, Test3 calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.impot_revenu, b.impot_revenu)
    );
}

function eq(Test3[] calldata a, Test3[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct CalculImpotRevenuIn {
    Personne personne_in;
}

function eq(CalculImpotRevenuIn calldata a, CalculImpotRevenuIn calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.personne_in, b.personne_in)
    );
}

function eq(CalculImpotRevenuIn[] calldata a, CalculImpotRevenuIn[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct CalculImpotDeuxTranchesIn {
    DeuxTranches tranches_in;
}

function eq(CalculImpotDeuxTranchesIn calldata a, CalculImpotDeuxTranchesIn calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.tranches_in, b.tranches_in)
    );
}

function eq(CalculImpotDeuxTranchesIn[] calldata a, CalculImpotDeuxTranchesIn[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

struct NouveauCalculImpotRevenuIn {
    Personne personne_in;
}

function eq(NouveauCalculImpotRevenuIn calldata a, NouveauCalculImpotRevenuIn calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.personne_in, b.personne_in)
    );
}

function eq(NouveauCalculImpotRevenuIn[] calldata a, NouveauCalculImpotRevenuIn[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

type Test1In is bool;

type Test2In is bool;

struct NouveauCalculImpotRevenuCorrectIn {
    Personne personne_in;
}

function eq(NouveauCalculImpotRevenuCorrectIn calldata a, NouveauCalculImpotRevenuCorrectIn calldata b)
    pure
    returns (bool)
{
    return(
        eq(a.personne_in, b.personne_in)
    );
}

function eq(NouveauCalculImpotRevenuCorrectIn[] calldata a, NouveauCalculImpotRevenuCorrectIn[] calldata b)
    pure
    returns (bool)
{
    if (a.length != b.length) return false;
    for (uint i = 0; i < a.length; i++) {
        if (!eq(a[i], b[i])) {
            return false;
        }
    }
    return true;
}

type Test3In is bool;
