package ast

import org.eclipse.jdt.core.dom._

/** Wraps ASTVisitor so scala's match-case pattern could be used instead
  */
trait ASTNodeVisitor extends ASTVisitor {
  protected def visit(node: ASTNode): Boolean

  private def visitInternal(node: ASTNode): Boolean = {
    try {
      visit(node.asInstanceOf[ASTNode])
    } catch {
      case _: MatchError ⇒ true
    }
  }

  override def visit(node: WildcardType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: AnnotationTypeDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: AnnotationTypeMemberDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: AnonymousClassDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ArrayAccess): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ArrayCreation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ArrayInitializer): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ArrayType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: AssertStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: Assignment): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: Block): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: BlockComment): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: BooleanLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: BreakStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: CastExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: CatchClause): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: CharacterLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ClassInstanceCreation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: CompilationUnit): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ConditionalExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ConstructorInvocation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ContinueStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: DoStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: EmptyStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: EnhancedForStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: EnumConstantDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: EnumDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ExpressionStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: FieldAccess): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: FieldDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ForStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: IfStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ImportDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: InfixExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: InstanceofExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: Initializer): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: Javadoc): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: LabeledStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: LineComment): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MarkerAnnotation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MemberRef): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MemberValuePair): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MethodRef): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MethodRefParameter): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MethodDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: MethodInvocation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: Modifier): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: NormalAnnotation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: NullLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: NumberLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: PackageDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ParameterizedType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ParenthesizedExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: PostfixExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: PrefixExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: PrimitiveType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: QualifiedName): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: QualifiedType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ReturnStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SimpleName): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SimpleType): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SingleMemberAnnotation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SingleVariableDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: StringLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SuperConstructorInvocation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SuperFieldAccess): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SuperMethodInvocation): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SwitchCase): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SwitchStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: SynchronizedStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TagElement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TextElement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ThisExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: ThrowStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TryStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TypeDeclaration): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TypeDeclarationStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TypeLiteral): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: TypeParameter): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: VariableDeclarationExpression): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: VariableDeclarationStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: VariableDeclarationFragment): Boolean = visitInternal(node.asInstanceOf[ASTNode])

  override def visit(node: WhileStatement): Boolean = visitInternal(node.asInstanceOf[ASTNode])
}
