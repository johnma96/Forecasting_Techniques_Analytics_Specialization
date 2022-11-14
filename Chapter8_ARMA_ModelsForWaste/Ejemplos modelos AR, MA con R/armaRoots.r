armaRoots = function (coefficients, n.plot = 400, digits = 4, ...) 
{
    root = polyroot(c(1, -coefficients))
    real.root = Re(root)
    im.root = Im(root)
    xrange = range(real.root)
    xrange = c(xrange[1] - 1.2 * abs(xrange[1]), xrange[2] + 
        1.2 * abs(xrange[2]))
    xplot = seq(xrange[1], xrange[2], length = n.plot)
    #fpoly = 1
    #for (i in 1:length(coefficients)) {
    #    fpoly = fpoly - xplot^i * coefficients[i]
    #}
    #plot(xplot, fpoly, type = "l", xlab = "B", ylab = "Function", 
    #    col = "steelblue", pch = 19, ...)
    #title(main = "Polynomial Function vs. B")
    #abline(h = 0)
    distance = sqrt(real.root^2 + im.root^2)
    root.mat = cbind(round(real.root, digits = digits), round(im.root, 
        digits = digits), round(distance, digits = digits))
    dimnames(root.mat) = list(1:nrow(root.mat), c("re", "im", 
        "dist"))
    size.limit = max(abs(real.root), 1.5, abs(im.root))
    plot(root, xlim = c(-size.limit, size.limit), ylim = c(-size.limit, 
        size.limit), xlab = "", ylab = "", col = "steelblue", 
        pch = 19, ...)
    x = (2 * pi/360) * (0:360)
    lines(sin(x), cos(x))
    abline(h = 0)
    abline(v = 0)
    title("Raices and Circulo Unitario", xlab = "Parte Real", ylab = "Parte  Imaginaria")
    result = root.mat
    data.frame(result)
}
