TEMPLATE := template.ml
BIN := run.out

# Créer un dossier d'exercice avec les fichiers vides
new:
	mkdir -p $(NAME)
	cp $(TEMPLATE) $(NAME)/main.ml
	touch $(NAME)/test.input $(NAME)/test.output $(NAME)/dataset.input
	@echo "✅ Créé le dossier $(NAME) avec les fichiers nécessaires."

# Compiler le programme
build:
	ocamlopt -o $(BIN) $(NAME)/main.ml

# Tester + résoudre le vrai dataset
run: build
	@echo "🔍 Test sur les données de test..."
	@bash -c '\
		OUTPUT=$$(./$(BIN) $(NAME)/test.input); \
		EXPECTED=$$(cat $(NAME)/test.output); \
		if [ "$$OUTPUT" = "$$EXPECTED" ]; then \
			echo "✅ Test passé."; \
		else \
			echo "❌ Test échoué."; \
			echo "--- Sortie obtenue ---"; \
			echo "$$OUTPUT"; \
			echo "--- Sortie attendue ---"; \
			echo "$$EXPECTED"; \
			exit 1; \
		fi'
	@echo "📦 Exécution sur le dataset réel..."
	@./$(BIN) $(NAME)/dataset.input > $(NAME)/dataset.output
	@echo "✅ Résultat écrit dans $(NAME)/dataset.output"

# Nettoyage
clean:
	@echo "🧹 Nettoyage des fichiers temporaires..."
	@rm -f $(BIN)
	@find . -type f \( -name "*.cmi" -o -name "*.cmo" -o -name "*.cmx" -o -name "*.o" -o -name "*.annot" \) -delete
	@find . -type f -name "dataset.output" -delete
	@find . -type f -name "dataset.input" -exec truncate -s 0 {} \;
	@echo "✅ Nettoyage terminé."

reset:
	@echo "♻️  Réinitialisation de $(NAME)..."
	@rm -f $(NAME)/dataset.output
	@truncate -s 0 $(NAME)/dataset.input
	@echo "✅ $(NAME) a été réinitialisé."

delete:
	@echo "🗑️  Suppression de l'exercice $(NAME)..."
	@if [ -d "$(NAME)" ]; then \
		rm -rf $(NAME); \
		echo "✅ Dossier $(NAME) supprimé."; \
	else \
		echo "⚠️  Aucun dossier nommé $(NAME) trouvé."; \
	fi

