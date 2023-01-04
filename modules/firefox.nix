# MacOS firefox configuration
{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.firefox = {
    enable = true;
    package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.firefox-bin else pkgs.firefox-wayland;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      add-custom-search-engine
      amp2html
      betterttv
      reddit-enhancement-suite
      tridactyl
      ublock-origin
      umatrix
    ];
  };

  programs.firefox.profiles =
    let
      userChrome = with config.lib.base16.theme; ''
        :root {
        --window-colour:                #${base01-hex};
        --secondary-colour:             #${base04-hex};
        --inverted-colour:              #${base05-hex};
        --uc-identity-color-blue:       #${base08-hex};
        --uc-identity-color-turquoise:  #${base09-hex};
        --uc-identity-color-green:      #${base0A-hex};
        --uc-identity-color-yellow:     #${base0B-hex};
        --uc-identity-color-orange:     #${base0C-hex};
        --uc-identity-color-red:        #${base0D-hex};
        --uc-identity-color-pink:       #${base0E-hex};
        --uc-identity-color-purple:     #${base0F-hex};
        /* URL colour in URL bar suggestions */
        --urlbar-popup-url-color: var(--uc-identity-color-purple) !important;
        /* global border radius */
        --uc-border-radius: 0;
        /* dynamic url bar width settings */
        --uc-urlbar-width: clamp(200px, 40vw, 500px);
        /* dynamic tab width settings */
        --uc-active-tab-width:   clamp(100px, 20vw, 300px);
        --uc-inactive-tab-width: clamp( 50px, 15vw, 200px);
        /* if active only shows the tab close button on hover*/
        --show-tab-close-button-hover: none; /* DEFAULT: -moz-inline-box; */
        /* adds left and right margin to the container-tabs indicator */
        --container-tabs-indicator-margin: 10px; }
        #back-button,
        #forward-button { display: none !important; }
        /* bookmark icon */
        #star-button{ display: none !important; }
        /* zoom indicator */
        #urlbar-zoom-button { display: none !important; }
        /* Make button small as Possible, hidden out of sight */
        #PanelUI-button { margin-top: -5px; margin-bottom: 44px; }
        #PanelUI-menu-button {
        padding: 0px !important;
        max-height: 1px;
        list-style-image: none !important;
        }
        #PanelUI-menu-button .toolbarbutton-icon { width: 1px !important; }
        #PanelUI-menu-button .toolbarbutton-badge-stack { padding: 0px !important; }
        #reader-mode-button{ display: none !important; }
        /* tracking protection shield icon */
        #tracking-protection-icon-container { display: none !important; }
        /* #identity-box { display: none !important } /* hides encryption AND permission items */
        #identity-permission-box { display: none !important; } /* only hodes permission items */
        /* e.g. playing indicator (secondary - not icon) */
        .tab-secondary-label { display: none !important; }
        #pageActionButton { display: none !important; }
        #page-action-buttons { display: none !important; }
        #urlbar-go-button { display: none !important; }
        :root {
        --uc-theme-colour:                          var(--window-colour);
        --uc-hover-colour:                          var(--secondary-colour);
        --uc-inverted-colour:                       var(--inverted-colour);
        --button-bgcolor:                           var(--uc-theme-colour)    !important;
        --button-hover-bgcolor:                     var(--uc-hover-colour)    !important;
        --button-active-bgcolor:                    var(--uc-hover-colour)    !important;
        --toolbar-bgcolor:                          var(--uc-theme-colour)    !important;
        --toolbarbutton-hover-background:           var(--uc-hover-colour)    !important;
        --toolbarbutton-active-background:          var(--uc-hover-colour)    !important;
        --toolbarbutton-border-radius:              var(--uc-border-radius)   !important;
        --lwt-toolbar-field-focus:                  var(--uc-theme-colour)    !important;
        --toolbarbutton-icon-fill:                  var(--uc-inverted-colour) !important;
        --toolbar-field-focus-background-color:     var(--secondary-colour)   !important;
        --toolbar-field-color:                      var(--uc-inverted-colour) !important;
        --toolbar-field-focus-color:                var(--uc-inverted-colour) !important;
        --tabs-border-color:                        var(--uc-theme-colour)    !important;
        --tab-border-radius:                        var(--uc-border-radius)   !important;
        --lwt-text-color:                           var(--uc-inverted-colour) !important;
        --lwt-tab-text:                             var(--uc-inverted-colour) !important;
        --lwt-sidebar-background-color:             var(--uc-hover-colour)    !important;
        --lwt-sidebar-text-color:                   var(--uc-inverted-colour) !important;
        --arrowpanel-border-color:                  var(--uc-theme-colour)    !important;
        --arrowpanel-border-radius:                 var(--uc-border-radius)   !important;
        --arrowpanel-background:                    var(--uc-theme-colour)    !important;
        --arrowpanel-color:                         var(--inverted-colour)    !important;
        --autocomplete-popup-highlight-background:  var(--uc-inverted-colour) !important;
        --autocomplete-popup-highlight-color:       var(--uc-inverted-colour) !important;
        --autocomplete-popup-hover-background:      var(--uc-inverted-colour) !important;
        --tab-block-margin: 2px !important;
        }
        window,
        #main-window,
        #toolbar-menubar,
        #TabsToolbar,
        #PersonalToolbar,
        #navigator-toolbox,
        #sidebar-box,
        #nav-bar {
        -moz-appearance: none !important;
        border: none !important;
        box-shadow: none !important;
        background: var(--uc-theme-colour) !important;
        }
        /* grey out ccons inside the toolbar to make it
        - more aligned with the Black & White colour look */
        #PersonalToolbar toolbarbutton:not(:hover),
        #bookmarks-toolbar-button:not(:hover) { filter: grayscale(1) !important; }
        /* remove window control buttons */
        .titlebar-buttonbox-container { display: none !important; }
        /* remove "padding" left and right from tabs */
        .titlebar-spacer { display: none !important; }
        /* remove gap after pinned tabs */
        #tabbrowser-tabs[haspinnedtabs]:not([positionpinnedtabs])
            > #tabbrowser-arrowscrollbox
            > .tabbrowser-tab[first-visible-unpinned-tab] { margin-inline-start: 0 !important; }
        /* remove tab shadow */
        .tabbrowser-tab
            >.tab-stack
            > .tab-background { box-shadow: none !important;  }
        /* tab background */
        .tabbrowser-tab
            > .tab-stack
            > .tab-background { background: var(--uc-theme-colour) !important; }
        /* active tab background */
        .tabbrowser-tab[selected]
            > .tab-stack
            > .tab-background { background: var(--uc-hover-colour) !important; }
        /* multi tab selection */
        #tabbrowser-tabs:not([noshadowfortests]) .tabbrowser-tab:is([visuallyselected=true], [multiselected]) > .tab-stack > .tab-background:-moz-lwtheme { background: var(--uc-hover-colour) !important; }
        /* tab close button options */
        .tabbrowser-tab:not([pinned]) .tab-close-button { display: var(--show-tab-close-button) !important; }
        .tabbrowser-tab:not([pinned]):hover .tab-close-button { display: var(--show-tab-close-button-hover) !important }
        /* adaptive tab width */
        .tabbrowser-tab[selected][fadein]:not([pinned]) { max-width: var(--uc-active-tab-width) !important; }
        .tabbrowser-tab[fadein]:not([selected]):not([pinned]) { max-width: var(--uc-inactive-tab-width) !important; }
        /* container tabs indicator */
        .tabbrowser-tab[usercontextid]
            > .tab-stack
            > .tab-background
            > .tab-context-line {
        margin: -1px var(--container-tabs-indicator-margin) 0 var(--container-tabs-indicator-margin) !important;
        border-radius: var(--tab-border-radius) !important;
        }
        /* show favicon when media is playing but tab is hovered */
        .tab-icon-image:not([pinned]) { opacity: 1 !important; }
        /* Makes the speaker icon to always appear if the tab is playing (not only on hover) */
        .tab-icon-overlay:not([crashed]),
        .tab-icon-overlay[pinned][crashed][selected] {
        top: 5px !important;
        z-index: 1 !important;
        padding: 1.5px !important;
        inset-inline-end: -8px !important;
        width: 16px !important; height: 16px !important;
        border-radius: 10px !important;
        }
        /* style and position speaker icon */
        .tab-icon-overlay:not([sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) {
        stroke: transparent !important;
        background: transparent !important;
        opacity: 1 !important; fill-opacity: 0.8 !important;
        color: currentColor !important;
        stroke: var(--uc-theme-colour) !important;
        background-color: var(--uc-theme-colour) !important;
        }
        /* change the colours of the speaker icon on active tab to match tab colours */
        .tabbrowser-tab[selected] .tab-icon-overlay:not([sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) {
        stroke: var(--uc-hover-colour) !important;
        background-color: var(--uc-hover-colour) !important;
        }
        .tab-icon-overlay:not([pinned], [sharing], [crashed]):is([soundplaying], [muted], [activemedia-blocked]) { margin-inline-end: 9.5px !important; }
        .tabbrowser-tab:not([image]) .tab-icon-overlay:not([pinned], [sharing], [crashed]) {
        top: 0 !important;
        padding: 0 !important;
        margin-inline-end: 5.5px !important;
        inset-inline-end: 0 !important;
        }
        .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        .tab-icon-overlay:not([crashed])[muted]:hover,
        .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover {
        color: currentColor !important;
        stroke: var(--uc-inverted-colour) !important;
        background-color: var(--uc-inverted-colour) !important;
        fill-opacity: 0.95 !important;
        }
        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[muted]:hover,
        .tabbrowser-tab[selected] .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover {
        color: currentColor !important;
        stroke: var(--uc-inverted-colour) !important;
        background-color: var(--uc-inverted-colour) !important;
        fill-opacity: 0.95 !important;
        }
        /* speaker icon colour fix */
        #TabsToolbar .tab-icon-overlay:not([crashed])[soundplaying],
        #TabsToolbar .tab-icon-overlay:not([crashed])[muted],
        #TabsToolbar .tab-icon-overlay:not([crashed])[activemedia-blocked] { color: var(--uc-inverted-colour) !important; }
        /* speaker icon colour fix on hover */
        #TabsToolbar .tab-icon-overlay:not([crashed])[soundplaying]:hover,
        #TabsToolbar .tab-icon-overlay:not([crashed])[muted]:hover,
        #TabsToolbar .tab-icon-overlay:not([crashed])[activemedia-blocked]:hover { color: var(--uc-theme-colour) !important; }
        #nav-bar {
        border:     none !important;
        box-shadow: none !important;
        background: transparent !important;
        }
        /* remove border below whole nav */
        #navigator-toolbox { border-bottom: none !important; }
        #urlbar,
        #urlbar * {
        outline: none !important;
        box-shadow: none !important;
        }
        #urlbar-background { border: var(--uc-hover-colour) !important; }
        #urlbar[focused="true"]
            > #urlbar-background,
        #urlbar:not([open])
            > #urlbar-background { background: transparent !important; }
        #urlbar[open]
            > #urlbar-background { background: var(--uc-theme-colour) !important; }
        .urlbarView-row:hover
            > .urlbarView-row-inner,
        .urlbarView-row[selected]
            > .urlbarView-row-inner { background: var(--uc-hover-colour) !important; }
        /* transition to oneline */
        @media (min-width: 1000px) {
        /* move tabs bar over */
        #TabsToolbar { margin-left: var(--uc-urlbar-width) !important; }
        /* move entire nav bar  */
        #nav-bar { margin: calc((var(--urlbar-min-height) * -1) - 8px) calc(100vw - var(--uc-urlbar-width)) 0 0 !important; }
        } /* end media query */
        /* Container Tabs */
        .identity-color-blue      { --identity-tab-color: var(--uc-identity-color-blue)      !important; --identity-icon-color: var(--uc-identity-color-blue)      !important; }
        .identity-color-turquoise { --identity-tab-color: var(--uc-identity-color-turquoise) !important; --identity-icon-color: var(--uc-identity-color-turquoise) !important; }
        .identity-color-green     { --identity-tab-color: var(--uc-identity-color-green)     !important; --identity-icon-color: var(--uc-identity-color-green)     !important; }
        .identity-color-yellow    { --identity-tab-color: var(--uc-identity-color-yellow)    !important; --identity-icon-color: var(--uc-identity-color-yellow)    !important; }
        .identity-color-orange    { --identity-tab-color: var(--uc-identity-color-orange)    !important; --identity-icon-color: var(--uc-identity-color-orange)    !important; }
        .identity-color-red       { --identity-tab-color: var(--uc-identity-color-red)       !important; --identity-icon-color: var(--uc-identity-color-red)       !important; }
        .identity-color-pink      { --identity-tab-color: var(--uc-identity-color-pink)      !important; --identity-icon-color: var(--uc-identity-color-pink)      !important; }
        .identity-color-purple    { --identity-tab-color: var(--uc-identity-color-purple)    !important; --identity-icon-color: var(--uc-identity-color-purple)    !important; }
      '';
      userContent = with config.lib.base16.theme; ''
        :root {
          scrollbar-width: none !important;
        }
        @-moz-document url(about:privatebrowsing) {
          :root {
            scrollbar-width: none !important;
          }
        }
         @-moz-document url("about:newtab"), url("about:home") {
          body {
            background-color: #${base01-hex} !important;
          }
          .search-wrapper .logo-and-wordmark .logo {
            background-image: url("https://raw.githubusercontent.com/NixOS/nixos-artwork/master/logo/nixos-white.png") !important;
            background-size: 100% !important;
            height: 250px !important;
            width: 500px !important;
          }
          .icon-settings,
          .body-wrapper,
          .SnippetBaseContainer,
          .search-handoff-button,
          .search-wrapper .logo-and-wordmark .wordmark,
          .search-wrapper .search-inner-wrapper,
          .search-wrapper input {
            display: none !important;
          }
        }
      '';

      settings = {
        "browser.ctrlTab.recentlyUsedOrder" = false;
        "browser.uidensity" = 1;
        "browser.urlbar.update1" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.trackingprotection.socialtracking.annotate.enabled" = true;
        "services.sync.declinedEngines" = "addons,prefs";
        "services.sync.engine.addons" = false;
        "services.sync.engineStatusChanged.addons" = true;
        "services.sync.engine.prefs" = false;
        "services.sync.engineStatusChanged.prefs" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "gfx.webrender.all" = true;
        "general.smoothScroll" = true;
      };
    in
    {
      home = {
        inherit settings;
        inherit userChrome;
        inherit userContent;
        id = 0;
      };
    };
}
