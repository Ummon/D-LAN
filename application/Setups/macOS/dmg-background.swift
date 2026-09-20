// Finder background in logical points; include a Retina representation.
// Keep dimensions and arrow placement in sync with dmg-layout.applescript.
import AppKit

let size = NSSize(width: 640, height: 360)
let image = NSImage(size: size)
for scale in [1, 2] {
    let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil,
        pixelsWide: Int(size.width) * scale, pixelsHigh: Int(size.height) * scale,
        bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
        isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
    bitmap.size = size
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmap)

    NSColor.white.setFill()
    NSBezierPath(rect: NSRect(origin: .zero, size: size)).fill()
    NSColor(red: 0.16, green: 0.35, blue: 0.53, alpha: 1).setFill()
    NSBezierPath(rect: NSRect(x: 0, y: 300, width: 640, height: 60)).fill()
    let text = "Drag D-LAN to Applications to install." as NSString
    let attributes: [NSAttributedString.Key: Any] = [
        .font: NSFont.systemFont(ofSize: 16, weight: .medium),
        .foregroundColor: NSColor.white
    ]
    let textSize = text.size(withAttributes: attributes)
    text.draw(at: NSPoint(x: (size.width - textSize.width) / 2,
                         y: 330 - textSize.height / 2), withAttributes: attributes)

    NSColor(red: 0.63, green: 0.68, blue: 0.73, alpha: 1).setStroke()
    let arrow = NSBezierPath()
    arrow.lineWidth = 5
    arrow.lineCapStyle = .round
    arrow.lineJoinStyle = .round
    arrow.move(to: NSPoint(x: 288, y: 190))
    arrow.line(to: NSPoint(x: 352, y: 190))
    arrow.move(to: NSPoint(x: 336, y: 206))
    arrow.line(to: NSPoint(x: 352, y: 190))
    arrow.line(to: NSPoint(x: 336, y: 174))
    arrow.stroke()

    NSGraphicsContext.restoreGraphicsState()
    image.addRepresentation(bitmap)
}
try image.tiffRepresentation!.write(to: URL(fileURLWithPath: CommandLine.arguments[1]))
