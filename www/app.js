$(document).ready(function () {
    // Layer Control Interactions (using delegation for robustness)
    $(document).on('mouseenter', '.map-layer-control', function () {
        $(this).addClass('expanded');
    });

    $(document).on('mouseleave', '.map-layer-control', function () {
        $(this).removeClass('expanded');
    });

    $(document).on('change', '.map-layer-control input[type="radio"]', function () {
        $('.map-layer-control').removeClass('expanded');
    });

    // Map loading overlay handlers
    Shiny.addCustomMessageHandler('show-map-overlay', function (msg) {
        $('#map_loading_overlay').addClass('active');
    });

    Shiny.addCustomMessageHandler('hide-map-overlay', function (msg) {
        $('#map_loading_overlay').removeClass('active');
    });

    // Freeze UI Handlers
    Shiny.addCustomMessageHandler('freezeUI', function (message) {
        console.log("Freezing UI", message);
        $('body').addClass('ui-frozen');
        $('.frozen-overlay').addClass('active');
        if (message && message.text) {
            $('.frozen-overlay-message').html(message.text);
        } else {
            $('.frozen-overlay-message').html('Synchronizing station data...');
        }
    });

    Shiny.addCustomMessageHandler('updateFreezeMsg', function (message) {
        if (message && message.text) {
            $('.frozen-overlay-message').html(message.text);
        }
    });

    Shiny.addCustomMessageHandler('unfreezeUI', function (message) {
        console.log("Unfreezing UI");
        $('body').removeClass('ui-frozen');
        $('.frozen-overlay').removeClass('active');
    });
});
