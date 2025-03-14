% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baConTree.R
\name{baConTree}
\alias{baConTree}
\title{Bayesian Context Tree R6 class}
\value{
Gets the sampled chain stored.
}
\description{
Bayesian Context Tree R6 class

Bayesian Context Tree R6 class
}
\section{Super class}{
\code{\link[bacontrees:ContextTree]{bacontrees::ContextTree}} -> \code{baConTree}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-baConTree-new}{\code{baConTree$new()}}
\item \href{#method-baConTree-setAllDirichletPars}{\code{baConTree$setAllDirichletPars()}}
\item \href{#method-baConTree-setContextPriorWeights}{\code{baConTree$setContextPriorWeights()}}
\item \href{#method-baConTree-runMetropolisHastings}{\code{baConTree$runMetropolisHastings()}}
\item \href{#method-baConTree-getChain}{\code{baConTree$getChain()}}
\item \href{#method-baConTree-clone}{\code{baConTree$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="activateByCode"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-activateByCode'><code>bacontrees::ContextTree$activateByCode()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="activateMaximal"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-activateMaximal'><code>bacontrees::ContextTree$activateMaximal()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="activateRoot"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-activateRoot'><code>bacontrees::ContextTree$activateRoot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="activeTreeCode"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-activeTreeCode'><code>bacontrees::ContextTree$activeTreeCode()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getActiveNodes"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getActiveNodes'><code>bacontrees::ContextTree$getActiveNodes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getAlphabet"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getAlphabet'><code>bacontrees::ContextTree$getAlphabet()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getChildrenNodes"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getChildrenNodes'><code>bacontrees::ContextTree$getChildrenNodes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getGrowableNodes"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getGrowableNodes'><code>bacontrees::ContextTree$getGrowableNodes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getLeaves"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getLeaves'><code>bacontrees::ContextTree$getLeaves()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getParentNode"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getParentNode'><code>bacontrees::ContextTree$getParentNode()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getPrunableNodes"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getPrunableNodes'><code>bacontrees::ContextTree$getPrunableNodes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="getSiblingNodes"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-getSiblingNodes'><code>bacontrees::ContextTree$getSiblingNodes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="growActive"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-growActive'><code>bacontrees::ContextTree$growActive()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="nodeExists"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-nodeExists'><code>bacontrees::ContextTree$nodeExists()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="print"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-print'><code>bacontrees::ContextTree$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="pruneActive"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-pruneActive'><code>bacontrees::ContextTree$pruneActive()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="root"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-root'><code>bacontrees::ContextTree$root()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="setData"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-setData'><code>bacontrees::ContextTree$setData()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bacontrees" data-topic="ContextTree" data-id="validate"><a href='../../bacontrees/html/ContextTree.html#method-ContextTree-validate'><code>bacontrees::ContextTree$validate()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-new"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$new(data, maximalDepth = 5, active = "root")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Either a vector with discrete data or a lista of vectors.}

\item{\code{maximalDepth}}{Depth of the maximal tree considered.}

\item{\code{active}}{Either "root" or "maximal" to indicate which nodes
should be initialized as active.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-setAllDirichletPars"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-setAllDirichletPars}{}}}
\subsection{Method \code{setAllDirichletPars()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$setAllDirichletPars(alpha)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{alpha}}{Hyperparameter considered for the Dirichlet prior distribution
of probabilities.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-setContextPriorWeights"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-setContextPriorWeights}{}}}
\subsection{Method \code{setContextPriorWeights()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$setContextPriorWeights(fn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fn}}{A function to be evaluated at each node that returns
its weight in the prior distribution.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-runMetropolisHastings"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-runMetropolisHastings}{}}}
\subsection{Method \code{runMetropolisHastings()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$runMetropolisHastings(steps)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{steps}}{Number of steps to run the Metropolis Hastings algorithm for.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This method supports progress monitoring via the \strong{progressr} package.
Users can wrap the function call in \code{with_progress()} to display a progress
bar while the function executes. If no progress handler is registered, the
function will run without showing progress.

To enable progress, register a handler and wrap the function call in
\code{with_progress()}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-getChain"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-getChain}{}}}
\subsection{Method \code{getChain()}}{
Chain generated via Metropolis Hastings algorithm.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$getChain()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-baConTree-clone"></a>}}
\if{latex}{\out{\hypertarget{method-baConTree-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{baConTree$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
