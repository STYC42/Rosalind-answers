TEMPLATE := template.ml
BIN := run.out

# Créer un dossier d'exercice avec les fichiers vides
create:
	@echo "Création du dossier $(NAME) et copie de template.ml..."
	@mkdir -p $(NAME)
	@cp template.ml $(NAME)/main.ml
	@echo "Entrez le contenu de $(NAME)/test.input (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.input
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.input; \
	done
	@echo "Entrez le contenu de $(NAME)/test.output (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.output
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.output; \
	done
	@echo "Création d'un fichier $(NAME)/dataset.input vide"
	@> $(NAME)/dataset.input
	@echo "✅ Dossier $(NAME) créé avec test et dataset.input vide."


# Compiler le programme
build:
	ocamlopt -o $(BIN) $(NAME)/main.ml

test: build
	@echo "🔍 Test sur les données de test..."
	@./run.out $(NAME)/test.input > $(NAME)/test.actual
	@diff -u $(NAME)/test.output $(NAME)/test.actual > $(NAME)/test.diff || true
	@if [ -s $(NAME)/test.diff ]; then \
		echo "❌ Test échoué. Différences :"; \
		cat $(NAME)/test.diff; \
		exit 1; \
	else \
		echo "✅ Test réussi."; \
		rm $(NAME)/test.actual $(NAME)/test.diff; \
	fi


# Tester + résoudre le vrai dataset
run: build test
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

