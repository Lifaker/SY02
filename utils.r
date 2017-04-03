#' Dessine les intervalles de confiance
#'
#' @param ICs Matrice des intervalles de confiance
#' @param mu Paramètre réel estimé par les ICs
#' @param plot Dessine les ICs ou pas
#' @param xlim Intervalle pour l'axe des abscisses
#' @param main Titre le graphique
plot_ICs <- function(ICs, mu, plot = TRUE, xlim, main) {
    nic <- ncol(ICs)
    hit <- ICs[1, ] < mu & ICs[2, ] > mu

    if (plot) {
        plot.new()
        if (missing(xlim)) xlim <- mu + c(-1, 1) * max(abs(ICs - mu))
        plot.window(xlim = xlim, ylim = c(0, nic - 1))
        axis(side = 1)
        segments(ICs[1, ], 1:nic, ICs[2, ], 1:nic, lwd = 2, col = 2 + hit)
        lines(c(mu, mu), c(0, nic), type = "l", lty = 2)
        if (!missing(main)) title(main = main)
    }
    # On renvoie le nombre d'intervalles couvrants sans en faire echo
    invisible(list(hit = sum(hit), miss = nic - sum(hit)))
}
