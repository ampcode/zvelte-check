<div
	style={`max-height: ${x + 2}px; ${`
		${(() => {
			if (position.rightAligned) {
				// position.right is a CSS right offset (distance from viewport right edge)
				// We need to check if there's enough space to the left of the anchor for the menu
				const leftSpaceFromAnchor = viewportWidth - (position.right || 0)
				if (leftSpaceFromAnchor >= menuWidth + 10) {
					// Enough space to the left of anchor for the menu
					return `left: auto; right: ${position.right}px; width: ${menuWidth}px;`
				} else {
					// Not enough space, switch to left alignment
					const leftPos = Math.max(10, viewportWidth - menuWidth - 10)
					return `left: ${leftPos}px; width: ${menuWidth}px;`
				}
			} else {
				// Left-aligned: ensure it doesn't go off right edge
				const maxLeft = Math.max(10, viewportWidth - menuWidth - 10)
				const leftPos = Math.min(position.left || 0, maxLeft)
				return `left: ${leftPos}px; width: ${menuWidth}px;`
			}
		})()}
	`}`}
>
	Content
</div>
