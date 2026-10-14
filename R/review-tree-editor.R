# Direct taxonomy editing. Commands operate on a reviewer draft, never transport.
.deduplicate_review_tree <- function(state) {
  s <- state
  n <- s$nodes
  root <- n$id[is.na(n$parent)]
  if(length(root)!=1L || anyDuplicated(n$id)) stop('The draft must have one root and unique category IDs.')
  key <- function(x) tolower(gsub('[[:space:]]+',' ',trimws(as.character(x))))
  children <- function(id) s$nodes$id[!is.na(s$nodes$parent) & s$nodes$parent==id]
  label <- function(id) s$nodes$label[match(id,s$nodes$id)]
  pending <- function(ids) {
    ids <- unique(ids);ids <- ids[!is.na(ids)]
    if(!length(ids)) return(invisible(NULL))
    s$affected <<- union(s$affected %||% character(),ids)
    s$resolved <<- setdiff(s$resolved %||% character(),ids)
    s$unresolved <<- union(s$unresolved %||% character(),ids)
    s$placements <<- s$placements[!s$placements$id %in% ids,,drop=FALSE]
    s$questions$leaf[s$questions$id %in% ids] <<- root
    invisible(NULL)
  }
  remove_node <- function(id) {
    s$nodes <<- s$nodes[s$nodes$id!=id,,drop=FALSE]
    s$reviewed <<- setdiff(s$reviewed %||% integer(),id)
    s$deferred <<- setdiff(s$deferred %||% integer(),id)
  }
  sibling_merges <- 0L; repeated_levels <- 0L
  repeat {
    changed <- FALSE
    parents <- unique(stats::na.omit(s$nodes$parent))
    for(parent in parents) {
      kids <- children(parent);groups <- split(kids,key(label(kids)))
      duplicate <- Filter(function(ids) length(ids)>1L,groups)
      if(!length(duplicate)) next
      ids <- duplicate[[1L]];keep <- ids[[1L]]
      for(remove in ids[-1L]) {
        s$nodes$parent[s$nodes$parent==remove] <- keep
        s$questions$leaf[s$questions$leaf==remove] <- keep
        remove_node(remove);sibling_merges <- sibling_merges+1L
      }
      if(length(children(keep))) pending(s$questions$id[s$questions$leaf==keep])
      changed <- TRUE;break
    }
    if(changed) next
    candidates <- s$nodes$id[!is.na(s$nodes$parent)]
    for(child in candidates) {
      parent <- s$nodes$parent[match(child,s$nodes$id)]
      if(!identical(key(label(child)),key(label(parent)))) next
      child_kids <- children(child);other_kids <- setdiff(children(parent),child)
      direct <- s$questions$id[s$questions$leaf==child]
      safe <- (length(child_kids)>0L && !length(direct)) ||
        (!length(child_kids) && !length(other_kids))
      if(!safe) next
      s$nodes$parent[s$nodes$parent==child] <- parent
      s$questions$leaf[s$questions$leaf==child] <- parent
      remove_node(child);repeated_levels <- repeated_levels+1L
      changed <- TRUE;break
    }
    if(!changed) break
  }
  pending_added <- length(setdiff(s$affected %||% character(),state$affected %||% character()))
  list(state=s,sibling_merges=sibling_merges,repeated_levels=repeated_levels,
       pending_questions=pending_added,changed=(sibling_merges+repeated_levels)>0L)
}

#' Apply an undoable direct taxonomy edit
#' @param state Reviewer draft containing nodes and questions.
#' @param command Edit command with action, target and action-specific fields.
#' @return Updated reviewer draft.
#' @export
edit_review_tree <- function(state, command) {
  s <- state
  action <- as.character(command$action)
  fields <- c('nodes','questions','reviewed','deferred','affected','resolved','unresolved','placements','new_count','next_node_id','history')
  snapshot <- function(x) x[intersect(fields,names(x))]
  restore <- function(x, value) {
    for (key in fields) x[[key]] <- value[[key]]
    x
  }
  if (action %in% c('undo','redo')) {
    from <- if(action=='undo') 'editor_undo' else 'editor_redo'
    to <- if(action=='undo') 'editor_redo' else 'editor_undo'
    stack <- s[[from]]
    if (!length(stack)) stop('There is no edit to ',action,'.')
    previous <- stack[[length(stack)]]
    s[[to]] <- c(s[[to]],list(snapshot(s)))
    s[[from]] <- head(stack,-1L)
    return(restore(s,previous))
  }
  if(action=='deduplicate') {
    result <- .deduplicate_review_tree(s)
    if(!result$changed) return(s)
    s <- result$state
    detail <- paste0('Automatic cleanup: ',result$sibling_merges,
      ' duplicate sibling categor',if(result$sibling_merges==1L) 'y' else 'ies',
      ' merged; ',result$repeated_levels,' repeated level',
      if(result$repeated_levels==1L) '' else 's',' collapsed; ',
      result$pending_questions,' question',if(result$pending_questions==1L) '' else 's',' marked pending')
    s$history <- rbind(s$history,data.frame(action=action,detail=detail))
    s$editor_undo <- c(s$editor_undo,list(snapshot(state)))
    s$editor_redo <- list()
    return(s)
  }
  n <- s$nodes
  root <- n$id[is.na(n$parent)]
  if(length(root)!=1L || anyDuplicated(n$id)) stop('The draft must have one root and unique category IDs.')
  target <- as.integer(command$target)
  if(length(target)!=1L || is.na(target) || !target %in% n$id) stop('Select an existing category.')
  children <- function(id) n$id[which(n$parent==id)]
  descendants <- function(id) c(id,unlist(lapply(children(id),descendants),use.names=FALSE))
  label <- function(id) n$label[match(id,n$id)]
  parent <- n$parent[match(target,n$id)]
  valid_name <- function(name, parent, except=integer()) {
    name <- trimws(name)
    if(length(name)!=1L || is.na(name) || !nzchar(name)) stop('Enter a category name.')
    others <- setdiff(children(parent),except)
    if(tolower(name) %in% tolower(trimws(label(others)))) stop('A sibling already has that name. Choose a distinct name or combine categories.')
    name
  }
  destination <- function() {
    d <- as.integer(command$destination)
    if(length(d)!=1L || is.na(d) || !d %in% n$id) stop('Choose an existing destination.')
    d
  }
  pending <- function(ids) {
    s$affected <<- union(s$affected,ids)
    s$resolved <<- setdiff(s$resolved,ids)
    s$unresolved <<- union(s$unresolved,ids)
    s$placements <<- s$placements[!s$placements$id %in% ids,,drop=FALSE]
    # Root is the explicit pending location in reviewer drafts, never published as an assignment.
    s$questions$leaf[s$questions$id %in% ids] <<- root
  }
  if(action=='rename') {
    name <- valid_name(command$label,parent,target)
    s$nodes$label[s$nodes$id==target] <- name
    detail <- paste(label(target),'→',name)
  } else if(action=='move') {
    d <- destination()
    if(target==root || d %in% descendants(target)) stop('A category cannot be moved into itself or its descendants.')
    valid_name(label(target),d,target)
    if(any(s$questions$leaf==d)) stop('The destination directly contains questions. Add an empty grouping category or move its questions first.')
    s$nodes$parent[s$nodes$id==target] <- d
    detail <- paste(label(target),'→',label(d))
  } else if(action=='add') {
    if(any(s$questions$leaf==target)) stop('This category directly contains questions. Add the grouping under its parent first.')
    name <- valid_name(command$label,target)
    id <- max(n$id,s$next_node_id %||% 0L)+1L
    row <- n[1,,drop=FALSE]; row[1,] <- NA
    row$id <- id; row$parent <- target; row$label <- name
    row$stable_id <- command$node_key %||% paste0('editor-',id)
    if(row$stable_id %in% n$stable_id) stop('Category identifier already exists.')
    s$nodes <- rbind(n,row);s$next_node_id <- id
    detail <- paste(name,'under',label(target))
  } else if(action=='remove_level') {
    if(target==root) stop('The root cannot be removed.')
    promoted <- children(target)
    names <- tolower(trimws(label(c(setdiff(children(parent),target),promoted))))
    if(anyDuplicated(names)) stop('Promoting these children would create duplicate sibling names. Rename or combine them first.')
    pending(s$questions$id[s$questions$leaf==target])
    s$nodes$parent[which(s$nodes$parent==target)] <- parent
    s$nodes <- s$nodes[s$nodes$id!=target,,drop=FALSE]
    detail <- paste('Removed',label(target),'; promoted its children; direct questions marked pending')
  } else if(action=='merge') {
    d <- destination()
    if(target==root || d==root || d %in% descendants(target) || target %in% descendants(d)) stop('Combine separate branches, not a category with its ancestor or descendant.')
    promoted <- children(target)
    names <- tolower(trimws(label(c(children(d),promoted))))
    if(anyDuplicated(names)) stop('The combined categories would have duplicate child names. Rename those children first.')
    direct <- s$questions$id[s$questions$leaf %in% c(target,d)]
    s$nodes$parent[which(s$nodes$parent==target)] <- d
    s$nodes <- s$nodes[s$nodes$id!=target,,drop=FALSE]
    if(length(c(children(d),promoted))) pending(direct) else s$questions$leaf[s$questions$leaf==target] <- d
    detail <- paste('Combined',label(target),'into',label(d))
  } else if(action=='flag') {
    s$deferred <- union(s$deferred,target)
    detail <- paste('Flagged',label(target),'for later')
  } else stop('Unknown taxonomy edit.')
  s$reviewed <- intersect(s$reviewed,s$nodes$id)
  s$deferred <- intersect(s$deferred,s$nodes$id)
  s$history <- rbind(s$history,data.frame(action=action,detail=detail))
  s$editor_undo <- c(s$editor_undo,list(snapshot(state)))
  s$editor_redo <- list()
  s
}
