library(DiagrammeR)

grViz('
digraph Modern_Mercantilism {
  graph [rankdir = TB,
         label = "Modern Mercantilism Framework",
         labelloc = t,
         fontsize = 20,
         nodesep = 0.4,
         ranksep = 0.4,
         newrank = true]
  
  // Central spine nodes
  node [shape = rectangle, style = filled, fillcolor = lightgrey, width = 2.5, height = 0.8]
  A [label = "Core Drivers"]
  B [label = "Key Dynamics"]
  C [label = "Primary Outcomes"]
  
  // Core Drivers
  node [fillcolor = lightblue, width = 2.5, height = 0.7]
  D1 [label = "Geopolitical Rivalry\nUS-China Trade War"]
  D2 [label = "Economic Insecurity\nInequality, Populism"]
  D3 [label = "Supply Chain Fragility\nConflict Related Shocks"]
  D4 [label = "Climate/Resource Pressures\nMineral Scrambles"]
  D5 [label = "Tech Disruption\nAI, Green Tech"]
  
  // Key Dynamics
  node [fillcolor = lightyellow, width = 2.5, height = 0.7]
  E1 [label = "Security Primacy\nEconomic Growth < National Security"]
  E2 [label = "Bloc Formation\nInfluence Spheres"]
  E3 [label = "Subsidy Race\nState-led Industrial Policy"]
  E4 [label = "Forced Decoupling\nSeparation"]
  E5 [label = "Industrial Resilience > Efficiency\nLocalized Supply Chains"]
  
  // Primary Outcomes
  node [fillcolor = lightpink, width = 2.5, height = 0.7]
  F1 [label = "Fragmented Globalization\nSlowbalization to Splinternet"]
  F2 [label = "Higher Inflation & Lower Growth\nDuplication & Protectionism"]
  F3 [label = "Tech Fragmentation\nCompeting AI/Chip Systems"]
  F4 [label = "Resource Nationalism\nMineral Conflicts"]
  F5 [label = "Geopolitical Risk\nChanges to Diplomatic Stratagies"]
  F6 [label = "Weakened Multilateralism\nBloc-Based Trade"]
  
  // Forecast nodes
  node [shape = box, style = "dashed,rounded,filled", fillcolor = white, width = 2.0, height = 0.6]
  FC1 [label = "Forecast #1\nChip Export Controls"]
  FC3 [label = "Forecast #3\nSubsidy Surge"]
  FC5 [label = "Forecast #2\nCost Pressures"]
  
  // Central spine connections
  edge [color = black, penwidth = 1.5]
  A -> B -> C
  
  // Driver connections
  edge [color = steelblue, dir = none]
  A -> {D1 D2 D3 D4 D5}
  
  // Dynamics connections
  edge [color = goldenrod, dir = none]
  B -> {E1 E2 E3 E4 E5}
  
  // Outcome connections
  edge [color = palevioletred, dir = none]
  C -> {F1 F2 F3 F4 F5 F6}
  
  // Forecast connections
  edge [color = gray50, style = dashed, arrowhead = none]
  E4 -> FC1
  E3 -> FC3
  F2 -> FC5
  
  // Vertical ordering
  { rank = same; A; }
  { rank = same; D1; D2; D3; D4; D5; }
  { rank = same; B; }
  { rank = same; E1; E2; E3; E4; E5; }
  { rank = same; C; }
  { rank = same; F1; F2; F3; F4; F5; F6; }
  
  // Alignment enforcement
  edge [style = invis, weight = 10]
  A -> B -> C
  D1 -> E1 -> F1
  D2 -> E2 -> F2
  D3 -> E3 -> F3
  D4 -> E4 -> F4
  D5 -> E5 -> F5
}')