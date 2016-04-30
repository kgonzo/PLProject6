
# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.8'

_lr_method = 'LALR'

_lr_signature = 'C92397F23C18EABC2202625B144E6329'
    
_lr_action_items = {'QUOTE':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[1,-17,-18,-21,-19,-16,-23,-22,1,-4,1,1,1,-10,-12,-11,-9,-5,-15,1,-13,]),'$end':([0,2,3,5,6,7,8,9,10,11,12,13,15,30,31,32,],[-20,-17,-18,-21,-2,-19,-16,-3,-23,-22,0,-1,-4,-5,-15,-14,]),'NUM':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[3,-17,-18,-21,-19,-16,-23,-22,3,-4,3,3,3,-10,-12,-11,-9,-5,-15,3,-13,]),'LPAREN':([0,1,2,3,5,7,8,10,11,14,15,16,17,18,19,21,22,23,24,30,31,33,36,],[4,14,-17,-18,-21,-19,-16,-23,-22,20,-4,20,20,28,20,-10,-12,-11,-9,-5,-15,20,-13,]),'TRUE':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[5,-17,-18,-21,-19,-16,-23,-22,5,-4,5,5,5,-10,-12,-11,-9,-5,-15,5,-13,]),'TEXT':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[7,-17,-18,-21,-19,-16,-23,-22,7,-4,7,7,7,-10,-12,-11,-9,-5,-15,7,-13,]),'RPAREN':([2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,25,26,27,29,30,31,33,34,35,36,],[-17,-18,-21,-19,-16,-23,-22,-8,-4,-8,-8,-8,-10,-7,-11,-9,30,31,32,-6,-5,-15,-8,36,-12,-13,]),'SIMB':([0,2,3,4,5,7,8,10,11,14,15,16,17,19,20,21,22,23,24,28,30,31,33,36,],[8,-17,-18,16,-21,-19,-16,-23,-22,8,-4,8,8,8,16,-10,-12,-11,-9,33,-5,-15,8,-13,]),'NIL':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[10,-17,-18,-21,-19,-16,-23,-22,10,-4,10,10,10,-10,-12,-11,-9,-5,-15,10,-13,]),'FALSE':([0,2,3,5,7,8,10,11,14,15,16,17,19,21,22,23,24,30,31,33,36,],[11,-17,-18,-21,-19,-16,-23,-22,11,-4,11,11,11,-10,-12,-11,-9,-5,-15,11,-13,]),'LET':([4,],[18,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'item':([14,16,17,19,33,],[19,19,19,19,34,]),'bool':([0,14,16,17,19,33,],[2,2,2,2,2,2,]),'quoted_list':([0,14,16,17,19,33,],[6,21,21,21,21,21,]),'list':([1,],[15,]),'empty':([14,16,17,19,33,],[22,22,22,22,35,]),'call':([0,14,16,17,19,33,],[9,23,23,23,23,23,]),'let':([4,],[17,]),'exp':([0,],[12,]),'atom':([0,14,16,17,19,33,],[13,24,24,24,24,24,]),'items':([14,16,17,19,],[25,26,27,29,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> exp","S'",1,None,None,None),
  ('exp -> atom','exp',1,'p_exp_atom','yacc.py',150),
  ('exp -> quoted_list','exp',1,'p_exp_qlist','yacc.py',154),
  ('exp -> call','exp',1,'p_exp_call','yacc.py',158),
  ('quoted_list -> QUOTE list','quoted_list',2,'p_quoted_list','yacc.py',162),
  ('list -> LPAREN items RPAREN','list',3,'p_list','yacc.py',168),
  ('items -> item items','items',2,'p_items','yacc.py',172),
  ('items -> empty','items',1,'p_items_empty','yacc.py',176),
  ('empty -> <empty>','empty',0,'p_empty','yacc.py',180),
  ('item -> atom','item',1,'p_item_atom','yacc.py',184),
  ('item -> quoted_list','item',1,'p_item_list','yacc.py',192),
  ('item -> call','item',1,'p_item_call','yacc.py',196),
  ('item -> empty','item',1,'p_item_empty','yacc.py',200),
  ('let -> LET LPAREN SIMB item RPAREN','let',5,'p_let','yacc.py',204),
  ('exp -> LPAREN let items RPAREN','exp',4,'p_let_exp','yacc.py',210),
  ('call -> LPAREN SIMB items RPAREN','call',4,'p_call','yacc.py',225),
  ('atom -> SIMB','atom',1,'p_atom_simbol','yacc.py',234),
  ('atom -> bool','atom',1,'p_atom_bool','yacc.py',238),
  ('atom -> NUM','atom',1,'p_atom_num','yacc.py',242),
  ('atom -> TEXT','atom',1,'p_atom_word','yacc.py',246),
  ('atom -> <empty>','atom',0,'p_atom_empty','yacc.py',250),
  ('bool -> TRUE','bool',1,'p_true','yacc.py',254),
  ('bool -> FALSE','bool',1,'p_false','yacc.py',258),
  ('atom -> NIL','atom',1,'p_nil','yacc.py',262),
]
