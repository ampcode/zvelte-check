<!--
  Vendor prefix compatibility.
  Tests detection of vendor-prefixed properties without standard counterparts.
-->
<script lang="ts">
    let expanded = false;
</script>

<div class="clamp-text">
    This text will be clamped to 3 lines using the -webkit-line-clamp property.
    When used without the standard line-clamp property, a warning should be shown.
</div>

<div class="clamp-text-ok">
    This text has both the prefixed and standard property, so no warning.
</div>

<style>
    /* Missing standard line-clamp property - should warn */
    /* Expected: line 21, message: Also define the standard property 'line-clamp' for compatibility */
    .clamp-text {
        display: -webkit-box;
        -webkit-line-clamp: 3;
        -webkit-box-orient: vertical;
        overflow: hidden;
    }

    /* Has both prefixed and standard - no warning */
    .clamp-text-ok {
        display: -webkit-box;
        -webkit-line-clamp: 4;
        line-clamp: 4;
        -webkit-box-orient: vertical;
        overflow: hidden;
    }
</style>
