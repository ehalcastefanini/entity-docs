<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é implementar uma interface gráfica para gerenciar as regras de checklist de documentos. Ele permite que os usuários visualizem, editem e configurem critérios e documentos associados a essas regras. O problema resolvido é a necessidade de uma interface centralizada e organizada para manipular essas informações de forma eficiente.

* **Tecnologias Utilizadas:**
  - Delphi (VCL) para desenvolvimento da interface gráfica.
  - Componentes de terceiros como `TsScrollBox`, `TsPageControl`, `TcxGrid` para estilização e funcionalidades avançadas.
  - Classes e frames personalizados como `TFRAMEdocsCheckRules`, `TFRAMEdocsCheckRulesCriteria`, e `TFRAMEdocsCheckRulesDocs`.

* **Forma do Componente:**
  - **Forma:** Este código implementa um formulário com abas (tabs) e frames para exibir e editar dados.
    - **Elementos do Formulário:**
      - `PNLdata`: Painel principal que contém os frames e abas.
      - `PGCdetails`: Controle de abas com duas páginas: "Criteria" e "Documents".
      - `FRAMEdocsCheckRules1`: Frame principal para exibição de informações gerais.
      - `FRAMEdocsCheckRulesCriteria1`: Frame para exibição e edição de critérios.
      - `FRAMEdocsCheckRulesDocs1`: Frame para exibição e edição de documentos.
    - **Ações do Formulário:**
      - Carregar dados ao inicializar.
      - Navegar entre as abas para visualizar critérios e documentos.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Carregar dados relacionados às regras de checklist de documentos.
  - Exibir informações gerais, critérios e documentos em abas separadas.
  - Permitir a edição de critérios e documentos.

* **Componentes Principais:**
  - `PNLdata`: Painel que organiza os frames e abas.
  - `PGCdetails`: Controle de abas para alternar entre critérios e documentos.
  - `FRAMEdocsCheckRules1`: Frame para exibir informações gerais.
  - `FRAMEdocsCheckRulesCriteria1`: Frame para gerenciar critérios.
  - `FRAMEdocsCheckRulesDocs1`: Frame para gerenciar documentos.

* **Tradução para Pseudo-código:**
  - Evento `OnCreate` do formulário: `Ao criar o formulário, inicializar a aba ativa como "Criteria".`
  - Método `m_getData`: 
    ```pseudo
    Se o formulário for carregado:
      Configurar fontes de dados mestre-detalhe para critérios e documentos.
      Permitir acesso a registros inativos.
      Chamar método herdado para carregar dados.
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. O formulário é inicializado (`FormCreate`).
  2. A aba "Criteria" é definida como ativa.
  3. O método `m_getData` é chamado para carregar os dados:
     - Configura as relações mestre-detalhe entre os frames.
     - Permite acesso a registros inativos.
     - Carrega os dados usando o método herdado.

* **Dados Necessários:**
  - Fonte de dados mestre para critérios e documentos.
  - Parâmetros de serviço para configurar a exibição de registros inativos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Carregar dados.
    - Pré-condição: O formulário deve ser inicializado.
  - Ação: Navegar entre abas.
    - Pré-condição: Dados devem estar carregados.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Aba ativa padrão: "Criteria".

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`m_CreateFormEdit`:**
  - Cria e retorna uma instância do formulário `TFORMEdocsCheckListRules`.

* **`m_getData`:**
  - Configura as relações mestre-detalhe entre os frames.
  - Permite acesso a registros inativos.
  - Carrega os dados usando o método herdado.

* **`FormCreate`:**
  - Define a aba "Criteria" como ativa ao criar o formulário.

---

## 6. Consumo de Serviços de API:

* Não há chamadas a serviços externos definidas no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `TsScrollBox`, `TsPageControl`, `TcxGrid`: Componentes de terceiros para estilização e funcionalidades avançadas.

* **Componentes Personalizados:**
  - `TFRAMEdocsCheckRules`, `TFRAMEdocsCheckRulesCriteria`, `TFRAMEdocsCheckRulesDocs`: Frames personalizados para exibição e edição de dados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Critérios (exibidos no frame `FRAMEdocsCheckRulesCriteria1`).
  - Documentos (exibidos no frame `FRAMEdocsCheckRulesDocs1`).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não definido explicitamente no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  - Exemplo de inicialização do formulário:
    ```delphi
    var
      Form: TFORMEdocsCheckListRules;
    begin
      Form := TFORMEdocsCheckListRules.Create(Application);
      Form.Show;
    end;
    ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* Configuração de relações mestre-detalhe no método `m_getData`:
  ```delphi
  FRAMEdocsCheckRulesCriteria1.MasterSource := lv_MasterFrame.DStable;
  FRAMEdocsCheckRulesDocs1.MasterSource := lv_MasterFrame.DStable;
  ```

* Permissão para acessar registros inativos:
  ```delphi
  lv_MasterFrame.ServiceParams.ShowInactives := True;
  ```

---

## 12. Conclusão:

O código implementa um formulário bem estruturado para gerenciar regras de checklist de documentos, com uma interface organizada em abas e frames. Sua principal limitação é a ausência de validações explícitas e mensagens de erro. No entanto, ele é extensível e utiliza componentes avançados para estilização e funcionalidade.

---

## 13. Resumo Curto:

O código implementa um formulário para gerenciar regras de checklist de documentos, com abas para critérios e documentos. Ele carrega dados mestre-detalhe e permite acesso a registros inativos, utilizando componentes personalizados e de terceiros para uma interface eficiente.#### **EdocsCheckListRules.pas**

```
unit EdocsCheckListRules;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFRGridEditSOA, kneFREditSOA,
  kneFRCtrlEditSOA, FRdocsCheckRules, sScrollBox, sPageControl,
  FRdocsCheckRulesDocs, FRdocsCheckRulesCriteria;

type
  TFORMEdocsCheckListRules = class(TFORMkneBaseEdit)
    PNLdata: TsScrollBox;
    FRAMEdocsCheckRules1: TFRAMEdocsCheckRules;
    PGCdetails: TsPageControl;
    TSHcriteria: TsTabSheet;
    TSHdocuments: TsTabSheet;
    FRAMEdocsCheckRulesDocs1: TFRAMEdocsCheckRulesDocs;
    FRAMEdocsCheckRulesCriteria1: TFRAMEdocsCheckRulesCriteria;
    procedure FormCreate(Sender: TObject);

  protected
    procedure m_getData; override;

  private
    { Private declarations }
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEdocsCheckListRules: TFORMEdocsCheckListRules;

implementation

uses
  kneUtils;

{$R *.dfm}

{ TFORMEdocsCheckListRules }

class function TFORMEdocsCheckListRules.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
  Result := TFORMEdocsCheckListRules.Create(Application);
end;

procedure TFORMEdocsCheckListRules.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;

  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  // setup das rela��es master-detail
  FRAMEdocsCheckRulesCriteria1.MasterSource := lv_MasterFrame.DStable;
  FRAMEdocsCheckRulesDocs1.MasterSource     := lv_MasterFrame.DStable;

  lv_MasterFrame.ServiceParams.ShowInactives := True; // para permitir aceder aos registos inativos

  inherited m_getData;

end;

procedure TFORMEdocsCheckListRules.FormCreate(Sender: TObject);
begin
  inherited;
  PGCdetails.ActivePageIndex := 0;
end;

end.
```

#### **EdocsCheckListRules.dfm**

```
inherited FORMEdocsCheckListRules: TFORMEdocsCheckListRules
  Height = 562
  Caption = 'Docs Check List Rules'
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PNLdata: TsScrollBox [2]
    Left = 0
    Top = 41
    Width = 784
    Height = 483
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL_LOW'
    inline FRAMEdocsCheckRules1: TFRAMEdocsCheckRules
      Left = 0
      Top = 0
      Width = 780
      Height = 120
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 86
        Width = 780
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 44
        Width = 780
        inherited GRPstatus: TsGroupBox
          Width = 780
          inherited DBTXTlastUpd: TsDBText
            Font.Color = 5059883
            DataSource = FRAMEdocsCheckRules1.DStable
          end
          inherited DBTXTupdBy: TsDBText
            Font.Color = 5059883
            DataSource = FRAMEdocsCheckRules1.DStable
          end
          inherited ICBOstat: TcxDBImageComboBox
            DataBinding.DataSource = FRAMEdocsCheckRules1.DStable
            Width = 97
          end
        end
      end
    end
    object PGCdetails: TsPageControl
      Left = 0
      Top = 120
      Width = 780
      Height = 359
      ActivePage = TSHcriteria
      Align = alClient
      TabOrder = 1
      SkinData.SkinSection = 'PAGECONTROL'
      object TSHcriteria: TsTabSheet
        Caption = 'Criteria'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckRulesCriteria1: TFRAMEdocsCheckRulesCriteria
          Left = 0
          Top = 0
          Width = 772
          Height = 331
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 772
            Height = 297
          end
          inherited PNLfooter: TsPanel
            Top = 297
            Width = 772
          end
        end
      end
      object TSHdocuments: TsTabSheet
        Caption = 'Documents'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckRulesDocs1: TFRAMEdocsCheckRulesDocs
          Left = 0
          Top = 0
          Width = 772
          Height = 331
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 772
            Height = 297
          end
```
<!-- tabs:end -->

