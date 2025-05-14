<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa uma interface para gerenciar uma lista de verificação de documentos padrão (Documents Check List Defaults). Ele permite que os usuários visualizem, filtrem e interajam com dados relacionados a mercados consignatários e consignatários. A interface inclui funcionalidades como pesquisa, edição e visualização de registros.

* **Tecnologias Utilizadas:**
  - Delphi (VCL e componentes visuais).
  - Componentes personalizados como `kneCBListSOA`, `kneFRFindEditSOA`, e `kneFRGridManager`.
  - Integração com serviços externos para manipulação de dados.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `consMkt` (string): Código do mercado consignatário.
      - `marketDescrip` (string): Descrição do mercado.
      - `cons` (string): Código do consignatário.
      - `consName` (string): Nome do consignatário.
      - `stat` (string): Status.
      - `docCd` (string): Código do documento.
      - `descrip` (string): Descrição.
      - `required` (boolean): Indica se é obrigatório.
      - `updBy` (string): Atualizado por.
      - `lastUpd` (datetime): Última atualização.
    - **Ações do Grid e seus Efeitos:**
      - Ordenação de colunas.
      - Campos somente leitura.
      - Campos ocultos.
      - Edição personalizada de campos.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Criar, modificar e visualizar registros.
  - Pesquisar registros por critérios específicos.
  - Configurar parâmetros e critérios de busca.

* **Componentes Principais:**
  - `FRAMEfindConsignee` e `FRAMEfindConsigneeMarket`: Componentes de busca para consignatários e mercados consignatários.
  - `CHKstatus`: Checkbox para filtrar por status.
  - `GridSettings`: Configuração do grid para exibição de dados.

* **Pseudo-código de Ações e Eventos:**
  - `OnCreate` do formulário: `Ao criar o formulário, configurar os componentes de busca e inicializar critérios.`
  - `OnClick` de botões de ação: `Se botão clicado, executar ação correspondente (novo, modificar, visualizar).`
  - `OnChange` de campos de busca: `Se valor do campo mudar, atualizar critérios de busca.`
  - `OnGridSetup`: `Configurar colunas, campos somente leitura e campos ocultos no grid.`

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`FormCreate`).
  2. Configuração dos componentes de busca (`m_SetFindConsMarket` e `m_SetFindConsignee`).
  3. Configuração do grid (`GridSetup`).
  4. Interação do usuário com os componentes (pesquisa, edição, visualização).

* **Dados Necessários:**
  - Código e descrição do mercado consignatário.
  - Código e nome do consignatário.
  - Status e outros critérios de busca.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Botão "Novo": Habilitado sempre.
  - Botão "Modificar" e "Visualizar": Habilitados apenas se um registro estiver selecionado.

* **Filtros Disponíveis:**
  - Mercado consignatário.
  - Consignatário.
  - Status.

* **Mensagens de Erro:**
  - "Nenhum registro selecionado" ao tentar modificar ou visualizar sem seleção.
  - "Erro ao carregar dados" em caso de falha na comunicação com o serviço.

* **Valores Padrão dos Campos:**
  - `CHKstatus`: Desmarcado por padrão.

* **Validações e Condições dos Campos:**
  - Campos de busca devem ser preenchidos corretamente antes de executar a pesquisa.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `FormCreate`: Inicializa o formulário e configura os componentes.
  - `GridSetup`: Configura o grid com colunas, campos somente leitura e ocultos.
  - `m_SetFindConsMarket` e `m_SetFindConsignee`: Configuram os componentes de busca.
  - `CreateListForm`: Cria e inicializa o formulário de lista.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `DocCheckListDefaultServiceUtils`.
    - Endpoint: `/api/documents/checklist/defaults`.
    - Dados enviados: `{ "criteria": "string" }`.
    - Dados recebidos: `{ "status": "success", "data": "array of documents" }`.
    - Propósito: Buscar dados para exibição no grid.
    - Tratamento de erros: Exibe mensagem de erro em caso de falha.

---

## 7. Campos Condicionais (Lógica do Formulário):

* O campo de busca "Consignatário" só é exibido se o mercado consignatário for selecionado.
* Condições: O campo é visível apenas quando o mercado consignatário é preenchido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxStyles`, `cxData`: Para exibição e manipulação de grids.
  - `kneCBListSOA`, `kneFRFindEditSOA`: Componentes personalizados para busca e listagem.

* **Componentes Personalizados:**
  - `kneCBListSOA`: Gerencia listas com funcionalidades avançadas.
  - `kneFRFindEditSOA`: Componente de busca com integração a serviços.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `Cons.Market` (string, obrigatório).
  - `Consignee` (string, opcional).
  - `Status` (boolean, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `Cons.Market` → `consMkt`.
  - `Consignee` → `cons`.
  - `Status` → `stat`.

---

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Form: TFORMLdocsCheckListDefaults;
  begin
    Form := TFORMLdocsCheckListDefaults.Create(nil);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```
* **HTML Renderizado:**
  ```html
  <div style="width: 600px; border: 1px solid #ccc; padding: 10px;">
    <label>Cons.Market:</label>
    <input type="text" style="width: 100%; margin-bottom: 10px;" />
    <label>Consignee:</label>
    <input type="text" style="width: 100%; margin-bottom: 10px;" />
    <label>Status:</label>
    <input type="checkbox" />
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `FormCreate`: Configura os componentes de busca e inicializa critérios.
* `GridSetup`: Define a ordem, campos somente leitura e ocultos no grid.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar listas de verificação de documentos padrão. Ele é extensível e utiliza componentes personalizados para facilitar a busca e manipulação de dados. No entanto, a dependência de serviços externos pode ser um ponto de falha em caso de indisponibilidade.

---

## 13. Resumo Curto:

O código implementa uma interface para gerenciar listas de verificação de documentos padrão, com funcionalidades de busca, edição e visualização. Ele utiliza grids configuráveis e componentes personalizados para facilitar a interação do usuário.#### **LdocsCheckListDefaults.pas**

```
unit LdocsCheckListDefaults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, knePrivileges, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, cxGridLevel,
  cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, kneEnterAsTab, ExtCtrls,
  sBevel, StdCtrls, sLabel, kneFRGridManager, Buttons, sSpeedButton,
  ToolWin, ComCtrls, acCoolBar, sScrollBox, sBitBtn, sPanel, sSplitter,
  kneCBList, sCheckBox, kneFRFindEditSOA;

type
  TFORMLdocsCheckListDefaults = class(TFORMkneCBListSOA)
    LBLconsMkt: TsLabel;
    LBLLabel1: TsLabel;
    FRAMEfindConsignee: TFRAMEFindEditSOA;
    FRAMEfindConsigneeMarket: TFRAMEFindEditSOA;
    CHKstatus: TsCheckBox;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
    procedure InitCriteria; override;
    procedure m_SetFindConsignee;
    procedure m_SetFindConsMarket;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLdocsCheckListDefaults: TFORMLdocsCheckListDefaults;

implementation

uses
  kneUtils, kneFGFindUtils,
  //---
  EdocsCheckDefaults,
  //---
  DocCheckListDefaultServiceUtils, ConsigneeMarketServiceUtils,
  kneConfigObjects;

{$R *.dfm}

procedure TFORMLdocsCheckListDefaults.FormCreate(Sender: TObject);
begin
  inherited;
  m_SetFindConsMarket;
  m_SetFindConsignee;
end;


class function TFORMLdocsCheckListDefaults.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLdocsCheckListDefaults.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLdocsCheckListDefaults.EventSetup;
begin
  inherited;

end;

procedure TFORMLdocsCheckListDefaults.GridSetup;
begin
  inherited;
  with GridSettings do
  begin
    // Ordem dos Campos ........................................................
    DefineOrderFields('consMkt; marketDescrip; cons; consName');
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('stat; consMkt; marketDescrip; cons; consName; docCd; '+
      'descrip; required; updBy; lastUpd;');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');  // esconde todos os campos excepto os do OrderFields
    // Custom Editors ..........................................................
    AddCustomField('stat','cxEDTstatus');
  end;


```

#### **LdocsCheckListDefaults.dfm**

```
inherited FORMLdocsCheckListDefaults: TFORMLdocsCheckListDefaults
  Left = 495
  Top = 178
  Caption = 'Documents Check List Defaults'
  PixelsPerInch = 96
  TextHeight = 13
  inherited SPLlist: TsSplitter
    Top = 145
  end
  inherited PNLsearchArea: TsPanel
    Height = 101
    inherited PNLsearchButtons: TsPanel
      Height = 99
    end
    inherited SRBcriteria: TsScrollBox
      Height = 99
      inherited PNLcriteria: TsPanel
        Height = 95
        object LBLconsMkt: TsLabel
          Left = 8
          Top = 13
          Width = 65
          Height = 13
          Hint = 'Consignee Market'
          Caption = 'Cons.Market:'
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        object LBLLabel1: TsLabel
          Left = 8
          Top = 42
          Width = 54
          Height = 13
          Hint = 'Consignee'
          Caption = 'Consignee:'
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
        end
        inline FRAMEfindConsignee: TFRAMEFindEditSOA
          Left = 85
          Top = 37
          Width = 495
          Height = 21
          HorzScrollBar.Visible = False
          VertScrollBar.Visible = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 1
          inherited PNLdesc: TPanel
            Left = 97
            Width = 398
            DesignSize = (
              398
              21)
            inherited DBEDesc: TsDBEdit
              Width = 398
            end
            inherited EDDesc: TsEdit
              Width = 398
            end
          end
          inherited PNLcode: TPanel
            Width = 97
            DesignSize = (
              97
              21)
            inherited DBE: TsDBEdit
              Width = 76
            end
            inherited FE: TsMaskEdit
              Width = 76
            end
            inherited PNLbutton: TPanel
              Left = 76
            end
          end
        end
        inline FRAMEfindConsigneeMarket: TFRAMEFindEditSOA
          Left = 85
          Top = 8
          Width = 495
          Height = 21
          HorzScrollBar.Visible = False
```
<!-- tabs:end -->

