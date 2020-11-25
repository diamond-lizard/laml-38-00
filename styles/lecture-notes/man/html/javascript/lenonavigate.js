// Navigation upon activation of the 'p', 'n', 'u', and 't' keys resp.
// Also '1', ..., '9' (downward navigagion).
// Double mouse click activates nextUrl.
// Works for IE4 and IE5 from Microsoft, but not for Netscape Navigator.
function lenoNavigate(prevUrl,nextUrl,upUrl,toggleUrl,downUrls){
  theEvent = window.event;
  if (theEvent.type == "keypress"){
    kc = theEvent.keyCode;
    if ((kc == 112) && (prevUrl != "")) window.location = prevUrl;      // p
    else if (((kc == 110) || (kc == 13)) && (nextUrl != "")) window.location = nextUrl; // n
    else if ((kc == 117) && (upUrl != "")) window.location = upUrl;   // u
    else if ((kc == 116) && (toggleUrl != "")) window.location = toggleUrl;   // t
    else if ((kc >= 49) && (kc <= 57)){
          idx = kc - 49;
          if ((idx + 1) <= downUrls.length)  
             window.location = downUrls[idx]; // 1 - 9
         }
    }
  else if (theEvent.type == "dblclick"){
    if (nextUrl != "") window.location = nextUrl;
    return(false);
  }
}

// Navigation upon activation of the 'P', 'N', and 'U' keys resp. Works for IE4 and IE5.
// Double mouse click activates nextUrl.
// For trail navigation FROM THE LEFTMOST TRAIL BAR ONLY
function lenoTrailbarNavigate(prevUrl,nextUrl,upUrl){
  theEvent = window.event;
  if (theEvent.type == "keypress"){
    kc = theEvent.keyCode;
    if ((kc == 80) && (prevUrl != "")) window.parent.location = prevUrl;      // P
    else if (((kc == 78) || (kc ==32)) && (nextUrl != "")) window.parent.location = nextUrl; // N
    else if (kc == 85) window.parent.location = upUrl;   // U
  }
  else if (theEvent.type == "dblclick"){
    if (nextUrl != "") window.parent.location = nextUrl;
    return(false);
  }
}

// Navigation upon activation of the 'P', 'N', and 'U' keys resp. Works for IE4 and IE5.
// Double mouse click activates nextUrl.
// For trail navigation FROM THE TRAIL FRAME ONLY (THE WHOLE):
function lenoTrailframeNavigate(prevUrl,nextUrl,upUrl){
  theEvent = window.event;
  if (theEvent.type == "keypress"){
    kc = theEvent.keyCode;
    if ((kc == 80) && (prevUrl != "")) window.location = prevUrl;      // P
    else if (((kc == 78) || (kc ==32)) && (nextUrl != "")) window.location = nextUrl; // N
    else if (kc == 85) window.location = upUrl;   // U
  }
  else if (theEvent.type == "dblclick"){
    if (nextUrl != "") window.location = nextUrl;
    return(false);
  }
}







