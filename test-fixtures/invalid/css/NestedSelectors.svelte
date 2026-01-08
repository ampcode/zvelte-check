<!--
  CSS nested selectors.
  Tests detection of unused selectors in nested CSS structures.
-->
<script lang="ts">
    let expanded = false;
</script>

<nav class="nav">
    <ul class="menu">
        <li class="item">
            <a href="/" class="link">Home</a>
        </li>
        <li class="item">
            <a href="/about" class="link">About</a>
        </li>
    </ul>
</nav>

<style>
    /* Used selectors in nested structure */
    .nav {
        background: #333;

        .menu {
            list-style: none;
            display: flex;

            .item {
                padding: 0.5rem;

                .link {
                    color: white;
                    text-decoration: none;

                    &:hover {
                        text-decoration: underline;
                    }
                }
            }
        }
    }

    /* Nested unused selector */
    /* Expected: line 46, code: css-unused-selector, message: Unused CSS selector ".nav .sidebar" */
    .nav {
        .sidebar {
            width: 200px;
        }
    }

    /* Unused class in nested context */
    /* Expected: line 54, code: css-unused-selector, message: Unused CSS selector ".menu .dropdown" */
    .menu {
        .dropdown {
            position: absolute;
        }
    }

    /* Deeply nested unused */
    /* Expected: line 62, code: css-unused-selector, message: Unused CSS selector ".nav .menu .item .icon" */
    .nav {
        .menu {
            .item {
                .icon {
                    width: 16px;
                }
            }
        }
    }

    /* Parent selector with unused */
    .link {
        &:focus {
            outline: 2px solid blue;
        }

        /* Expected: line 78, code: css-unused-selector, message: Unused CSS selector ".link.external" */
        &.external {
            color: orange;
        }
    }
</style>
