(************************************************************************)
(*                                TzScan                                *)
(*                                                                      *)
(*  Copyright 2017-2018 OCamlPro                                        *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU General Public  *)
(*  License as published by the Free Software Foundation; either        *)
(*  version 3 of the License, or (at your option) any later version.    *)
(*                                                                      *)
(*  TzScan is distributed in the hope that it will be useful,           *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

open Ocp_js

type options = {
  untrustedSpec: bool;
  suppressWarnings: bool;
  hideHostname: bool;
  requiredPropsFirst: bool;
  sortPropsAlphabetically: bool;
  showExtensions: bool;
  noAutoAuth: bool;
  pathInMiddlePanel: bool;
  hideLoading: bool;
  nativeScrollbars: bool;
  hideDownloadButton: bool;
  disableSearch: bool;
  onlyRequiredInSamples: bool;
}

let default_options = {
  untrustedSpec = false;
  suppressWarnings = false;
  hideHostname = false;
  requiredPropsFirst = true;
  sortPropsAlphabetically = false;
  showExtensions = false;
  noAutoAuth = false;
  pathInMiddlePanel = true;
  hideLoading = false;
  nativeScrollbars = true;
  hideDownloadButton = true;
  disableSearch = false;
  onlyRequiredInSamples = false;
}


class type options_obj = object
  method untrustedSpec: bool Js.t Js.prop
  method suppressWarnings: bool Js.t Js.prop
  method hideHostname: bool Js.t Js.prop
  method requiredPropsFirst: bool Js.t Js.prop
  method sortPropsAlphabetically: bool Js.t Js.prop
  method showExtensions: bool Js.t Js.prop
  method noAutoAuth: bool Js.t Js.prop
  method pathInMiddlePanel: bool Js.t Js.prop
  method hideLoading: bool Js.t Js.prop
  method nativeScrollbars: bool Js.t Js.prop
  method hideDownloadButton: bool Js.t Js.prop
  method disableSearch: bool Js.t Js.prop
  method onlyRequiredInSamples: bool Js.t Js.prop
end

let build_option js_options options =
  if options.untrustedSpec then
    js_options##untrustedSpec <- Js.bool options.untrustedSpec;
  if options.suppressWarnings then
    js_options##suppressWarnings <- Js.bool options.suppressWarnings;
  if options.hideHostname then
    js_options##hideHostname <- Js.bool options.hideHostname;
  if options.requiredPropsFirst then
    js_options##requiredPropsFirst <- Js.bool options.requiredPropsFirst;
  if options.sortPropsAlphabetically then
    js_options##sortPropsAlphabetically <- Js.bool options.sortPropsAlphabetically;
  if options.showExtensions then
    js_options##showExtensions <- Js.bool options.showExtensions;
  if options.noAutoAuth then
    js_options##noAutoAuth <- Js.bool options.noAutoAuth;
  if options.pathInMiddlePanel then
    js_options##pathInMiddlePanel <- Js.bool options.pathInMiddlePanel;
  if options.hideLoading then
    js_options##hideLoading <- Js.bool options.hideLoading;
  if options.nativeScrollbars then
    js_options##nativeScrollbars <- Js.bool options.nativeScrollbars;
  if options.hideDownloadButton then
    js_options##hideDownloadButton <- Js.bool options.hideDownloadButton;
  if options.disableSearch then
    js_options##disableSearch <- Js.bool options.disableSearch;
  if options.onlyRequiredInSamples then
    js_options##onlyRequiredInSamples <- Js.bool options.onlyRequiredInSamples

let init ?(options=default_options) (json : string)
    (container : [> Html_types.div ] elt) =
  let js_options : options_obj Js.t = Js.Unsafe.obj [||] in
  build_option js_options options;
  Js.Unsafe.global##_Redoc##init(Js.string json, js_options, To_dom.of_div container)
