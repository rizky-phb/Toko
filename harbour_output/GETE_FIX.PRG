// ============================================================
// File    : GETE_FIX.PRG
// Custom GETE() function replacement
// In CA-Clipper, GETE() reads an environment variable.
// In Harbour, use GetEnv() directly.
// This file provides backward-compatible GETE() wrapper.
// ============================================================

FUNCTION GETE( cVarName )
   RETURN GetEnv( cVarName )
