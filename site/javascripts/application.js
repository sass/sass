if (window.location.pathname == "/") {
    $(document).ready(function() {
        var v = $(".feature").addClass("scss").children("p").
            after("<div class='tabs'><div class='scss'>.scss</div><div class='sass'>.sass</div></div>").
            parent().find(".tabs div").click(function(event) {
                var tab = $(this);
                tab.parents(".feature").removeClass("scss").removeClass("sass").
                    addClass(this.className);
            });
    });
}
