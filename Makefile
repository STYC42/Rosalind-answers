TEMPLATE := template.ml
BIN := run.out

# Créer un dossier d'exercice avec les fichiers vides
create:
	@echo "📁 Création du dossier $(NAME) et copie de template.ml..."
	@mkdir -p $(NAME)
	@cp template.ml $(NAME)/main.ml
	@echo "(executable (name main))" > $(NAME)/dune
	@echo "(lang dune 3.18)" > $(NAME)/dune-project
	@echo "✍️  Entrez le contenu de $(NAME)/test.input (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.input
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.input; \
	done
	@echo "✍️  Entrez le contenu de $(NAME)/test.output (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.output
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.output; \
	done
	@echo "✅ Dossier $(NAME) prêt avec test.input et test.output."



# Compiler le programme
build:
	@cd $(NAME) && dune build
	@cp $(NAME)/_build/default/main.exe $(BIN) && chmod u+w $(BIN)

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

test_all:
	@for d in */ ; do \
		case $$d in \
			.*) ;; \
			*) DIR=$${d%/}; \
			   printf "Testing %s... " "$$DIR"; \
			   dune build --root $$DIR > /dev/null 2>&1 || { \
			      echo "Fail (build error)"; exit 1; }; \
			   ./$$DIR/_build/default/main.exe $$DIR/test.input > $$DIR/test.actual 2>/dev/null || { \
			      echo "Fail (execution error)"; exit 1; }; \
			   diff -u $$DIR/test.output $$DIR/test.actual > $$DIR/test.diff || true; \
			   if [ -s $$DIR/test.diff ]; then \
			     echo "Fail"; \
			     cat $$DIR/test.diff; \
			     exit 1; \
			   else \
			     echo "Done"; \
			     rm -f $$DIR/test.actual $$DIR/test.diff; \
			   fi; \
		esac; \
	done; \
	echo "All tests passed."


# Tester + résoudre le vrai dataset
run: build test
	@echo "📦 Entrez le dataset réel (ligne vide pour terminer) :"
	@rm -f $(NAME)/dataset.output
	@rm -f $(NAME)/dataset.input
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/dataset.input; \
	done
	@./run.out $(NAME)/dataset.input > $(NAME)/dataset.output
	@echo "✅ Résultat écrit dans $(NAME)/dataset.output"


# Nettoyage
clean:
	@echo "🧹 Nettoyage des fichiers temporaires..."
	@rm -f $(BIN)
	@find . -type f \( -name "*.cmi" -o -name "*.cmo" -o -name "*.cmx" -o -name "*.o" -o -name "*.annot" \) -delete
	@find . -type f -name "dataset.output" -delete
	@find . -type f -name "dataset.input" -delete


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

