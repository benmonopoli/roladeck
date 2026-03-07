[@mel.scope ("navigator", "clipboard")]
external writeText: string => Js.Promise.t(unit) = "writeText";
