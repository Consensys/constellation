-- Copyright (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

module Constellation.Util.Tls.Vincent where

import Data.ASN1.Types
import Data.Char
import Data.Maybe
import Data.X509
import Data.X509.Validation

validateCertificateName :: HostName -> Certificate -> [FailedReason]
validateCertificateName fqhn cert
    | not $ null altNames =
        findMatch [] $ map matchDomain altNames
    | otherwise =
        case commonName of
            Nothing -> [NoCommonName]
            Just cn -> findMatch [] $ [matchDomain cn]
  where (commonName, altNames) = getNames cert

        findMatch :: [FailedReason] -> [[FailedReason]] -> [FailedReason]
        findMatch _   []      = [NameMismatch fqhn]
        findMatch _   ([]:_)  = []
        findMatch acc (_ :xs) = findMatch acc xs

        matchDomain :: String -> [FailedReason]
        matchDomain name = case splitDot name of
            l | any (== "") l       -> [InvalidName name]
              | head l == "*"       -> wildcardMatch (drop 1 l)
              | l == splitDot fqhn  -> [] -- success: we got a match
              | otherwise           -> [NameMismatch fqhn]

        -- A wildcard matches a single domain name component.
        --
        -- e.g. *.server.com will match www.server.com but not www.m.server.com
        --
        -- Only 1 wildcard is valid and only for the left-most component. If
        -- used at other positions or if multiples are present
        -- they won't have a wildcard meaning but will be match as normal star
        -- character to the fqhn and inevitably will fail.
        --
        -- e.g. *.*.server.com will try to litteraly match the '*' subdomain of server.com
        --
        -- Also '*' is not accepted as a valid wildcard
        wildcardMatch l
            | null l                      = [InvalidWildcard] -- '*' is always invalid
            | l == drop 1 (splitDot fqhn) = [] -- success: we got a match
            | otherwise                   = [NameMismatch fqhn]

        splitDot :: String -> [String]
        splitDot [] = [""]
        splitDot x  =
            let (y, z) = break (== '.') x in
            map toLower y : (if z == "" then [] else splitDot $ drop 1 z)

getNames :: Certificate -> (Maybe String, [String])
getNames cert = (commonName >>= asn1CharacterToString, altNames)
  where commonName = getDnElement DnCommonName $ certSubjectDN cert
        altNames   = maybe [] toAltName $ extensionGet $ certExtensions cert
        toAltName (ExtSubjectAltName names) = catMaybes $ map unAltName names
            where unAltName (AltNameDNS s) = Just s
                  unAltName _              = Nothing
