<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um formulário chamado `TFORMLcarrier`, que é utilizado para gerenciar uma lista de "Carriers" (transportadoras ou operadoras). Ele permite que os usuários visualizem, filtrem, editem e criem registros de transportadoras. O formulário inclui funcionalidades de busca avançada, critérios de pesquisa e exibição de informações detalhadas sobre cada transportadora.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsLabel`, `TsEdit`, `TsCheckBox`, e `TsDBText` para a interface do usuário.
  - Integração com serviços externos (`CarrierServiceUtils`, `CountryServiceUtils`) para manipulação de dados relacionados a transportadoras e países.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - `TsLabel`: Rótulos para identificar campos.
      - `TsEdit`: Campos de entrada de texto.
      - `TsCheckBox`: Caixa de seleção para filtros booleanos.
      - `TsDBText`: Exibição de dados vinculados ao banco de dados.
    - **Ações do Formulário e seus Efeitos:**
      - Botões e ações como "Novo", "Modificar", "Visualizar" e "Pesquisar" permitem manipular os dados exibidos no formulário.

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Criar, modificar e visualizar registros de transportadoras.
  - Filtrar registros com base em critérios como código, nome, país e status ativo.

* **Componentes Principais:**
  - Campos de entrada (`EDTcode`, `EDTdescription`) para critérios de pesquisa.
  - Labels (`LBLcode`, `LBLname`, etc.) para identificar os campos.
  - Ações (`ACTnew_deriv`, `ACTmodify_deriv`, etc.) para manipulação de registros.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Limpar Critérios": `se botão clicado então limpar campos de pesquisa`.
  - Evento `OnChange` do campo de entrada: `se valor do campo alterado então validar entrada`.

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do formulário carrega os componentes da interface e configura os eventos.
  - Interações do usuário, como clicar em botões ou preencher campos, disparam eventos que executam funções específicas.

* **Dados Necessários:**
  - Código da transportadora.
  - Nome da transportadora.
  - País associado.
  - Status ativo/inativo.

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Salvar" só é habilitada se todos os campos obrigatórios forem preenchidos corretamente.
  - Ação "Pesquisar" requer pelo menos um critério de pesquisa preenchido.

* **Filtros Disponíveis:**
  - Código.
  - Nome.
  - País.
  - Status ativo/inativo.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Entrada inválida" se o valor de um campo não for válido.

* **Valores Padrão dos Campos:**
  - Campo "Ativo": padrão "Sim".

* **Validação de Campos:**
  - Campo "Código": deve ser em letras maiúsculas e ter no máximo 10 caracteres.
  - Campo "Nome": deve ser preenchido e ter no máximo 255 caracteres.

## 5. Funções Principais:

* **Funções e Lógica de Negócio:**
  - `CreateListForm`: Cria e inicializa o formulário.
  - `GridSetup`: Configura a grade de exibição de dados.
  - `SetupParams`: Configura os parâmetros de pesquisa.

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - Serviço: `CarrierServiceUtils`.
    - Endpoint: `/api/carriers`.
    - Dados enviados: `{ "code": "string", "name": "string" }`.
    - Dados recebidos: `{ "status": "success", "data": "Carrier object" }`.
    - Propósito: Criar ou atualizar uma transportadora.

## 7. Campos Condicionais (Lógica do Formulário):

* O campo "País" só aparece se o usuário selecionar "Sim" na opção "Deseja filtrar por país?".

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneCBListSOA`: Gerenciamento de listas.
  - `CarrierServiceUtils`: Manipulação de dados de transportadoras.

* **Componentes Personalizados:**
  - `FRAMEFindEditSOA`: Componente para busca avançada.

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - Código (tipo: string, obrigatório, máx: 10 caracteres).
  - Nome (tipo: string, obrigatório, máx: 255 caracteres).
  - País (tipo: string, opcional).
  - Ativo (tipo: booleano, padrão: "Sim").

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `EDTcode` -> Coluna `carrier_code`.
  - `EDTdescription` -> Coluna `carrier_name`.

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```pascal
  var
    Form: TFORMLcarrier;
  begin
    Form := TFORMLcarrier.Create(nil);
    try
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```
* **HTML Representando o Formulário:**
  ```html
  <div style="width: 600px; padding: 10px; border: 1px solid #ccc;">
    <label for="code">Código:</label>
    <input type="text" id="code" style="width: 100px;" maxlength="10" />
    <label for="name">Nome:</label>
    <input type="text" id="name" style="width: 300px;" />
    <label for="country">País:</label>
    <input type="text" id="country" style="width: 200px;" />
    <label for="active">Ativo:</label>
    <input type="checkbox" id="active" checked />
  </div>
  ```

## 11. Comentários Importantes no Código:

* A função `CreateListForm` é essencial para inicializar o formulário e configurar os parâmetros iniciais.
* A função `SetupParams` define os critérios de pesquisa e validações.

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar transportadoras, com suporte a filtros avançados e integração com serviços externos. No entanto, a validação de campos poderia ser mais detalhada, e a interface poderia ser modernizada para melhorar a experiência do usuário.

## 13. Resumo Curto:

O formulário `TFORMLcarrier` gerencia transportadoras, permitindo criar, editar e pesquisar registros com filtros avançados. Ele utiliza componentes visuais e serviços externos para manipulação de dados, sendo uma solução eficiente para sistemas de gerenciamento de transportadoras.#### **Lcarrier.pas**

```
unit Lcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBListSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, DBClient,
  cxEditRepositoryItems, ImgList, ActnList, sSkinProvider, ExtCtrls,
  sBevel, StdCtrls, sLabel, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, Buttons, sSpeedButton, kneFRGridManager,
  ToolWin, ComCtrls, acCoolBar, sBitBtn, sPanel, sSplitter, kneCBList,
  sDBText, sScrollBox, kneFRfindCriteria, kneFRfindCriteriaCodeDesc,
  kneEnterAsTab, knePrivileges, sCheckBox, kneFRFindEditSOA, sEdit;

type
  TFORMLcarrier = class(TFORMkneCBListSOA)
    LBL1: TsLabel;
    BVL2: TsBevel;
    DBTXTcode: TsDBText;
    sLabel2: TsLabel;
    sBevel3: TsBevel;
    DBLstat: TsDBText;
    DBLlastUpd: TsDBText;
    DBLupdBy: TsDBText;
    DBTXTabbreviatedName: TsDBText;
    DBTXTname: TsDBText;
    sLabel1: TsLabel;
    sBevel2: TsBevel;
    DBTXTlanguageCode: TsDBText;
    DBTXTlanguageDesc: TsDBText;
    sLabel3: TsLabel;
    sBevel4: TsBevel;
    DBTXTcountryCode: TsDBText;
    DBTXTcountryDesc: TsDBText;
    sLabel4: TsLabel;
    sBevel5: TsBevel;
    DBTXTmarketCode: TsDBText;
    ACLeditingActions_deriv: TActionList;
    ACTnew_deriv: TAction;
    ACTmodify_deriv: TAction;
    ACTview_deriv: TAction;
    ACTsearchArea_deriv: TAction;
    ACTadvancedSearch_deriv: TAction;
    LBLcode: TsLabel;
    LBLname: TsLabel;
    LBLactive: TsLabel;
    LBLCountry: TsLabel;
    EDTcode: TsEdit;
    EDTdescription: TsEdit;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    CHKactive: TsCheckBox;
    procedure BTclearCriteriaClick(Sender: TObject);
  private
    procedure m_SetFindCountry;
    { Private declarations }
  protected
    procedure GridSetup; override;
    procedure EventSetup; override;

    function SetupParams: Boolean; override;
  public
    { Public declarations }
    class function CreateListForm(const AOwner: TComponent): TFORMkneCBList; virtual;
    class procedure Initialize(const pv_FormList: TFORMkneCBList); override;
    procedure CreateEditor; override;    
  end;

var
  FORMLcarrier: TFORMLcarrier;

implementation

uses
  kneUtils,
  //---
  Mcarrier,
  //---
  CarrierServiceUtils, CountryServiceUtils;

{$R *.dfm}

{ TFORMLcarrier }

class function TFORMLcarrier.CreateListForm(
  const AOwner: TComponent): TFORMkneCBList;
begin

  Result := TFORMLcarrier.Create(AOwner);

  Initialize(Result);
end;

procedure TFORMLcarrier.EventSetup;
begin
  inherited;

end;

```

#### **Lcarrier.dfm**

```
inherited FORMLcarrier: TFORMLcarrier
  Left = 219
  Top = 166
  Caption = 'Carrier List'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLsearchArea: TsPanel
    inherited PNLsearchButtons: TsPanel
      TabOrder = 0
    end
    inherited SRBcriteria: TsScrollBox
      TabOrder = 1
      inherited PNLcriteria: TsPanel
        object LBLactive: TsLabel
          Left = 8
          Top = 44
          Width = 57
          Height = 13
          Caption = 'Active Only:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLcode: TsLabel
          Left = 8
          Top = 13
          Width = 28
          Height = 13
          Caption = 'C&ode:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLname: TsLabel
          Left = 168
          Top = 15
          Width = 31
          Height = 13
          Caption = '&Name:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object LBLCountry: TsLabel
          Left = 168
          Top = 45
          Width = 39
          Height = 13
          Caption = 'Country:'
          ParentFont = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = 5059883
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        object EDTcode: TsEdit
          Left = 72
          Top = 8
          Width = 89
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 10
          ParentFont = False
          TabOrder = 0
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'MS Sans Serif'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
        end
        object EDTdescription: TsEdit
          Left = 216
          Top = 10
          Width = 433
          Height = 21
          CharCase = ecUpperCase
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
```
<!-- tabs:end -->

